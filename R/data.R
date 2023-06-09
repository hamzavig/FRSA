#' @title yieldCurves
#' @description This datasheet holds predefined Yield Curves.
#' @format A data frame with 2 rows and 13 variables:
#' \describe{
#'   \item{\code{rfType}}{character Indicates risk factor type.}
#'   \item{\code{label}}{character Adds a label/name to the risk factor object.}
#'   \item{\code{referenceDate}}{double Adds valid from date to the risk factor object.}
#'   \item{\code{tenor.1}}{character tenor.n defines the time span or length of time of rate.n to be valid }
#'   \item{\code{rate.1}}{double rate.n defines the interest rate for given tenor.n.}
#'   \item{\code{tenor.2}}{character tenor.n defines the time span or length of time of rate.n to be valid}
#'   \item{\code{rate.2}}{double rate.n defines the interest rate for given tenor.n.}
#'   \item{\code{tenor.3}}{character tenor.n defines the time span or length of time of rate.n to be valid}
#'   \item{\code{rate.3}}{double rate.n defines the interest rate for given tenor.n.}
#'   \item{\code{tenor.4}}{character tenor.n defines the time span or length of time of rate.n to be valid}
#'   \item{\code{rate.4}}{double rate.n defines the interest rate for given tenor.n.}
#'}
#' @source \url{https://github.com/hamzavig/FRSA/}
"yieldCurves"

#' @title defaultCurves
#' @description This datasheet holds predefined Default Curves.
#' @format A data frame with 2 rows and 17 variables:
#' \describe{
#'   \item{\code{rfType}}{character Indicates risk factor type.}
#'   \item{\code{label}}{character Adds a label/name to the risk factor object.}
#'   \item{\code{referenceDate}}{double Adds valid from date to the risk factor object.}
#'   \item{\code{tenor.1}}{character tenor.n defines the time span or length of time of rate.n to be valid }
#'   \item{\code{rate.1}}{double rate.n defines the interest rate for given tenor.n.}
#'   \item{\code{tenor.2}}{character tenor.n defines the time span or length of time of rate.n to be valid}
#'   \item{\code{rate.2}}{double rate.n defines the interest rate for given tenor.n.}
#'   \item{\code{tenor.3}}{character tenor.n defines the time span or length of time of rate.n to be valid}
#'   \item{\code{rate.3}}{double rate.n defines the interest rate for given tenor.n.}
#'   \item{\code{tenor.4}}{character tenor.n defines the time span or length of time of rate.n to be valid}
#'   \item{\code{rate.4}}{double rate.n defines the interest rate for given tenor.n.}
#'   \item{\code{tenor.5}}{character tenor.n defines the time span or length of time of rate.n to be valid}
#'   \item{\code{rate.5}}{double rate.n defines the interest rate for given tenor.n.}
#'   \item{\code{tenor.6}}{character tenor.n defines the time span or length of time of rate.n to be valid}
#'   \item{\code{rate.6}}{double rate.n defines the interest rate for given tenor.n.}
#'   \item{\code{tenor.7}}{character tenor.n defines the time span or length of time of rate.n to be valid}
#'   \item{\code{rate.7}}{double rate.n defines the interest rate for given tenor.n.}
#'}
#' @source \url{https://github.com/hamzavig/FRSA/}
"defaultCurves"

#' @title annuities
#' @description This datasheet holds predefined financial contracts forming a portfolio of annuities for an institution.
#' @format A data frame with 27 rows and 36 variables:
#' \describe{
#'   \item{\code{node}}{character The node defines the leaf of the financial instrument.}
#'   \item{\code{calendar}}{character Calendar defines the non-working days which affect the dates of contract events.}
#'   \item{\code{businessDayConvention}}{character Business Day Convention is linked to the calendar which defines working and non-working days. On this basis the BDC defines how to treat them (no shift / shift of calculation).}
#'   \item{\code{endOfMonthConvention}}{character End of Month Convention defines handling of the end of month in case of anchoor dates.}
#'   \item{\code{contractType}}{character The Contract Type defines the cash flow generating pattern of a contract.}
#'   \item{\code{statusDate}}{character Status Date holds the date per which all attributes of the record were updated.}
#'   \item{\code{contractRole}}{character Contract Role defines if the contract is an asset or liability, a long or short position.}
#'   \item{\code{legalEntityIDRecordCreator}}{character Identifies the legal entity creating the contract record.}
#'   \item{\code{contractID}}{character Unique identifier of a contract.}
#'   \item{\code{legalEntityIDCounterparty}}{character Identifies the counterparty of the contract}
#'   \item{\code{cycleAnchorDateOfInterestPayment}}{character Date from which the interest payment date schedule is calculated according to the cycle length. The first interest payment event takes place on this anchor.}
#'   \item{\code{cycleOfInterestPayment}}{character Cycle according to which the interest payment date schedule will be calculated.}
#'   \item{\code{nominalInterestRate}}{character The nominal interest rate which will be used to calculate accruals and the next interest payment at the next interest payment date.}
#'   \item{\code{dayCountConvention}}{character Method defining how days are counted between two dates. This finally defines the year fraction in accrual calculations.}
#'   \item{\code{accruedInterest}}{character Accrued interest as per status date.}
#'   \item{\code{cyclePointOfInterestPayment}}{character Defines if interest is paid at the beginning or the end of the cycle.}
#'   \item{\code{currency}}{character The currency of the cash flows.}
#'   \item{\code{amortizationDate}}{character Amortization Date is used to calculate the annuity amounts.}
#'   \item{\code{contractDealDate}}{character Contract Deal Date signifies the origination of the contract where an agreement between the customer and the bank has been settled.}
#'   \item{\code{initialExchangeDate}}{character Date of the initial cash flow for Maturity and Non-Maturity contracts. It also coincides with the beginning of interest accrual calculation.}
#'   \item{\code{premiumDiscountAtIED}}{character Total original premium or discount. Negative value for discount and positive for premium.}
#'   \item{\code{maturityDate}}{character Marks the contractual end of the lifecycle of a contract. Generally, date of the last cash flows.}
#'   \item{\code{notionalPrincipal}}{character Current nominal value of the contract.}
#'   \item{\code{cycleAnchorDateOfPrincipalRedemption}}{character Date from which the principal payment date schedule is calculated according to the cycle length. The first principal payment event takes place on this anchor.}
#'   \item{\code{cycleOfPrincipalRedemption}}{character Cycle according to which the interest payment date schedule will be calculated.}
#'   \item{\code{nextPrincipalRedemptionPayment}}{character Amount of principal that will be paid during the redemption cycle at the next payment date.}
#'   \item{\code{terminationDate}}{character If a contract is sold before MD (for example a bond on the secondary market) this date has to be set.}
#'   \item{\code{cycleAnchorDateOfRateReset}}{character Date from which the rate reset date schedule is calculated according to the cycle length. The first rate reset event takes place on this anchor.}
#'   \item{\code{cycleOfRateReset}}{character Cycle according to which the rate reset date schedule will be calculated.}
#'   \item{\code{rateSpread}}{character Interest rate spread. A typical rate resetting rule is LIBOR plus x basis point where x represents the interest rate spread.}
#'   \item{\code{marketObjectCodeOfRateReset}}{character Is pointing to the interest rate driver (MarketObject) used for rate reset uniquely. Unique codes for market objects must be used.}
#'   \item{\code{cyclePointOfRateReset}}{character Defines if rate reset happens at the beginning or at the end of the cycle.}
#'   \item{\code{rateMultiplier}}{character Interest rate multiplier.}
#'   \item{\code{description}}{character Description of the financial contract.}
#'   \item{\code{contrStrucObj.marketObjectCode}}{character tbd}
#'   \item{\code{contrStruc.referenceType}}{character tbd}
#'   \item{\code{contrStruc.referenceRole}}{character tbd} 
#'}
#' @source \url{https://www.actusfrf.org/dictionary/}
"annuities"

#' @title principalAtMaturities
#' @description This datasheet holds predefined financial contracts forming a portfolio of principalAtMaturities for an institution.
#' @format A data frame with 27 rows and 36 variables:
#' \describe{
#'   \item{\code{node}}{character The node defines the leaf of the financial instrument.}
#'   \item{\code{calendar}}{character Calendar defines the non-working days which affect the dates of contract events.}
#'   \item{\code{businessDayConvention}}{character Business Day Convention is linked to the calendar which defines working and non-working days. On this basis the BDC defines how to treat them (no shift / shift of calculation).}
#'   \item{\code{endOfMonthConvention}}{character End of Month Convention defines handling of the end of month in case of anchoor dates.}
#'   \item{\code{contractType}}{character The Contract Type defines the cash flow generating pattern of a contract.}
#'   \item{\code{statusDate}}{character Status Date holds the date per which all attributes of the record were updated.}
#'   \item{\code{contractRole}}{character Contract Role defines if the contract is an asset or liability, a long or short position.}
#'   \item{\code{legalEntityIDRecordCreator}}{character Identifies the legal entity creating the contract record.}
#'   \item{\code{contractID}}{character Unique identifier of a contract.}
#'   \item{\code{legalEntityIDCounterparty}}{character Identifies the counterparty of the contract}
#'   \item{\code{cycleAnchorDateOfInterestPayment}}{character Date from which the interest payment date schedule is calculated according to the cycle length. The first interest payment event takes place on this anchor.}
#'   \item{\code{cycleOfInterestPayment}}{character Cycle according to which the interest payment date schedule will be calculated.}
#'   \item{\code{nominalInterestRate}}{character The nominal interest rate which will be used to calculate accruals and the next interest payment at the next interest payment date.}
#'   \item{\code{dayCountConvention}}{character Method defining how days are counted between two dates. This finally defines the year fraction in accrual calculations.}
#'   \item{\code{accruedInterest}}{character Accrued interest as per status date.}
#'   \item{\code{cyclePointOfInterestPayment}}{character Defines if interest is paid at the beginning or the end of the cycle.}
#'   \item{\code{currency}}{character The currency of the cash flows.}
#'   \item{\code{amortizationDate}}{character Amortization Date is used to calculate the annuity amounts.}
#'   \item{\code{contractDealDate}}{character Contract Deal Date signifies the origination of the contract where an agreement between the customer and the bank has been settled.}
#'   \item{\code{initialExchangeDate}}{character Date of the initial cash flow for Maturity and Non-Maturity contracts. It also coincides with the beginning of interest accrual calculation.}
#'   \item{\code{premiumDiscountAtIED}}{character Total original premium or discount. Negative value for discount and positive for premium.}
#'   \item{\code{maturityDate}}{character Marks the contractual end of the lifecycle of a contract. Generally, date of the last cash flows.}
#'   \item{\code{notionalPrincipal}}{character Current nominal value of the contract.}
#'   \item{\code{cycleAnchorDateOfPrincipalRedemption}}{character Date from which the principal payment date schedule is calculated according to the cycle length. The first principal payment event takes place on this anchor.}
#'   \item{\code{cycleOfPrincipalRedemption}}{character Cycle according to which the interest payment date schedule will be calculated.}
#'   \item{\code{nextPrincipalRedemptionPayment}}{character Amount of principal that will be paid during the redemption cycle at the next payment date.}
#'   \item{\code{terminationDate}}{character If a contract is sold before MD (for example a bond on the secondary market) this date has to be set.}
#'   \item{\code{cycleAnchorDateOfRateReset}}{character Date from which the rate reset date schedule is calculated according to the cycle length. The first rate reset event takes place on this anchor.}
#'   \item{\code{cycleOfRateReset}}{character Cycle according to which the rate reset date schedule will be calculated.}
#'   \item{\code{rateSpread}}{character Interest rate spread. A typical rate resetting rule is LIBOR plus x basis point where x represents the interest rate spread.}
#'   \item{\code{marketObjectCodeOfRateReset}}{character Is pointing to the interest rate driver (MarketObject) used for rate reset uniquely. Unique codes for market objects must be used.}
#'   \item{\code{cyclePointOfRateReset}}{character Defines if rate reset happens at the beginning or at the end of the cycle.}
#'   \item{\code{rateMultiplier}}{character Interest rate multiplier.}
#'   \item{\code{description}}{character Description of the financial contract.}
#'   \item{\code{contrStrucObj.marketObjectCode}}{character tbd}
#'   \item{\code{contrStruc.referenceType}}{character tbd}
#'   \item{\code{contrStruc.referenceRole}}{character tbd} 
#'}
#' @source \url{https://www.actusfrf.org/dictionary/}
"principalAtMaturities"


#' @title operations
#' @description This datasheet holds predefined financial contracts forming a portfolio of operations for an institution.
#' @format A data frame with 10 rows and 14 variables:
#' \describe{
#'   \item{\code{node}}{character The node defines the leaf of the financial instrument.}
#'   \item{\code{contractType}}{character The Contract Type defines the cash flow generating pattern of a contract.}
#'   \item{\code{contractID}}{character Unique identifier of a contract.}
#'   \item{\code{contractRole}}{character Contract Role defines if the contract is an asset or liability, a long or short position.}
#'   \item{\code{currency}}{character The currency of the cash flows.}
#'   \item{\code{notionalPrincipal}}{character Current nominal value of the contract.}
#'   \item{\code{nominalInterestRate}}{character The nominal interest rate which will be used to calculate accruals and the next interest payment at the next interest payment date.}
#'   \item{\code{initialExchangeDate}}{character Date of the initial cash flow for Maturity and Non-Maturity contracts. It also coincides with the beginning of interest accrual calculation.}
#'   \item{\code{maturityDate}}{character Marks the contractual end of the lifecycle of a contract. Generally, date of the last cash flows.}
#'   \item{\code{repetition}}{numeric}
#'   \item{\code{frequency}}{numeric}
#'   \item{\code{times}}{numeric}
#'   \item{\code{inverted}}{logical}
#'   \item{\code{description}}{character Description of the operation contract.}
#'   
#'}
#' @source \url{https://www.actusfrf.org/dictionary/}
"operations"