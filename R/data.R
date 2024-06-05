#' Simulated Measles outbreak in Moïssala (Chad)
#'
#' This linelist replicates a measles outbreak over several regions of Southern Chad.
#' It uses realistic distributions and parameters from a measles outbreak.
#'
#' @format ## `moissala_measles`
#' A data frame with 5,028 rows and 30 columns:
#' \describe{
#'   \item{id}{epi id}
#'   \item{site}{Site name}
#'   \item{case_name}{Case name}
#'   \item{sex}{Case sex}
#'   \item{age}{Case age}
#'   \item{age_unit}{Age units}
#'   \item{age_group}{Age group}
#'   \item{region}{Case region of residence}
#'   \item{sub_prefecture}{Case sub-prefecture of residence}
#'   \item{date_onset}{Date of symptoms onset}
#'   \item{hospitalisation}{Hospitalisation status}
#'   \item{date_admission}{ Date of hospital admission}
#'   \item{ct_value}{Ct value of RT-PCR}
#'   \item{malaria_rdt}{Malaria RDT result}
#'   \item{fever}{presence of fever upon admission}
#'   \item{rash}{presence of rash upon admission}
#'   \item{cough}{presence of cough upon admission}
#'   \item{red_eye}{presence of red eyes upon admission}
#'   \item{pneumonia}{presence of pneumonia upon admission}
#'   \item{encephalitis}{presence of encephalitis upon admission}
#'   \item{muac}{Middle Upper Arm Circumference (MUAC) upon admission}
#'   \item{muac_cat}{MUAC category}
#'   \item{vacc_status}{Vaccination status}
#'   \item{vacc_doses}{Vaccine doses received}
#'   \item{outcome}{Outcome}
#'   \item{date_death}{Date of death}
#'   \item{date_exit}{Date of hospital exit}
#'   \item{epi_classification}{Epidemiological classification}
#' }
#' @source <https://epicentre-msf.github.io/gallery/>
"moissala_measles"
