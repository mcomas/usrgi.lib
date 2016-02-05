#' Returns the icd code for a disease
#' 
#' @param disease name of disease
#' @param f_xml xml file with the definitions
#' @param icd_list 
#' @param icd
#' @return coefficient for the incidence
#' 
#' @export
get_icd = function(disease, f_xml, icd_list, icd = 10){
  l_icd = c()
  meta_diseases_in = getNodeSet(f_xml, sprintf("/problems/disease[@name='%s']/meta[not(@action='exclude')]/text()", disease))
  if(length(meta_diseases_in)>0){
    meta_diseases_in_str = sapply(meta_diseases_in, function(el) xmlValue(el))
    l_icd = c(l_icd, unlist(sapply(meta_diseases_in_str, function(name_disease) get_icd(name_disease, f_xml, icd_list, icd))))
  }
  icds_in = getNodeSet(f_xml, sprintf("/problems/disease[@name='%s']/icd[@edition='icd%d' and not(@action='exclude')]", disease, icd))
  if(length(icds_in) > 0){
    l_icd = c(l_icd, unlist(sapply(icds_in, function(el) icd_filter(icd_list, xmlValue(el)))))
  }
  icds_out = getNodeSet(f_xml, sprintf("/problems/disease[@name='%s']/icd[@edition='icd%d' and @action='exclude']", disease, icd))
  if(length(icds_out) > 0){
    l_icd = setdiff(l_icd, unlist(sapply(icds_out, function(el) icd_filter(icd_list, xmlValue(el)))))
  }
  meta_diseases_out = getNodeSet(f_xml, sprintf("/problems/disease[@name='%s']/meta[@action='exclude']/text()", disease))
  if(length(meta_diseases_out)>0){
    meta_diseases_out_str = sapply(meta_diseases_out, function(el) xmlValue(el))
    l_icd = setdiff(l_icd, unlist(sapply(meta_diseases_out_str, function(name_disease) get_icd(name_disease, f_xml, icd_list, icd))))
  }
  unname(l_icd)
}