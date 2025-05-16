Financing <- R6::R6Class(
  classname = "Financing"
)

Mortgage <- R6::R6Class(
  classname = "Mortgage",
  inherit = Financing,
  active = list(
    purchase_price = function(value) {
      if (missing(value)) {
        return(private$.purchase_price)
      } else {
        check_number_decimal(value, arg = "purchase_price")
        private$.purchase_price = value
      }
    },
    down_payment = function(value) {
      if (missing(value)) {
        return(private$.down_payment)
      } else {
        check_number_decimal(value, arg = "down_payment")
        private$.down_payment = value
      }
    },
    interest_rate = function(value) {
      if (missing(value)) {
        return(private$.interest_rate)
      } else {
        check_number_decimal(value, min = 0, max = 1, arg = "interest_rate")
        private$.interest_rate = value
      }
    },
    loan_term = function(value) {
      if (missing(value)) {
        return(private$.loan_term)
      } else {
        check_number_whole(value, arg = "loan_term")
        private$.loan_term = value
      }
    },
    amortization_table = function(value) {
      if (missing(value)) {
        private$calculate_amort_table()
      } else {
        stop("amortization_table is read-only")
      }
    },
    monthly_payment = function(value) {
      if (missing(value)) {
        private$calculate_payment(round_digits = 2)
      } else {
        stop("monthly_payment is read-only")
      }
    }
  ),
  private = list(
    .purchase_price = numeric(),
    .down_payment = numeric(),
    .interest_rate = numeric(),
    .loan_term = numeric(),

    calculate_amort_table = function() {
      monthly_interest <- self$interest_rate / 12
      num_payments <- self$loan_term * 12
      monthly_payment <- private$calculate_payment()

      payment_number <- 1:num_payments
      payment <- numeric(num_payments)
      interest <- numeric(num_payments)
      principal <- numeric(num_payments)
      balance <- numeric(num_payments)

      remaining_balance <- self$purchase_price - self$down_payment

      for (t in 1:num_payments) {
        interest[t] <- remaining_balance * monthly_interest
        principal[t] <- monthly_payment - interest[t]
        remaining_balance <- remaining_balance - principal[t]
        payment[t] <- round(interest[t], 2) + round(principal[t], 2)
        balance[t] <- remaining_balance
      }

      amortization_table <- data.frame(
        month = payment_number,
        total_payment = round(payment, 2),
        interest_payment = round(interest, 2),
        principal_payment = round(principal, 2),
        remaining_balance = round(pmax(balance, 0), 2)
      )

      return(amortization_table)
    },

    calculate_payment = function(round_digits = NULL) {
      check_number_whole(round_digits, allow_null = TRUE)
      num_payments <- self$loan_term * 12
      monthly_interest <- self$interest_rate / 12
      monthly_payment <- .calculate_monthly_payment(
        P = self$purchase_price - self$down_payment,
        i = monthly_interest,
        n = num_payments
      )
      if (is.null(round_digits)) {
        return(monthly_payment)
      } else {
        return(round(monthly_payment, round_digits))
      }
    }
  ),
  public = list(
    initialize = function(purchase_price,
                          down_payment,
                          interest_rate,
                          loan_term) {
      self$purchase_price <- purchase_price
      self$down_payment <- down_payment
      self$interest_rate <- interest_rate
      self$loan_term <- loan_term

      invisible(self)
    }
  )
)

.calculate_monthly_payment <- function(P, i, n) {
  A <- (i * P * (1 + i)^n) / ((1 + i)^n - 1)
  return(A)
}
