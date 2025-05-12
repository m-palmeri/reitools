Mortgage <- R6::R6Class(
  classname = "Mortgage",
  active = list(
    purchase_price = function(value) {
      if (missing(value)) {
        return(private$.purchase_price)
      } else {
        check_number_decimal(value)
        private$.purchase_price = value
      }
    },
    down_payment = function(value) {
      if (missing(value)) {
        return(private$.down_payment)
      } else {
        check_number_decimal(value)
        private$.down_payment = value
      }
    },
    interest_rate = function(value) {
      if (missing(value)) {
        return(private$.interest_rate)
      } else {
        check_number_decimal(value, min = 0, max = 1)
        private$.interest_rate = value
      }
    },
    loan_term = function(value) {
      if (missing(value)) {
        return(private$.loan_term)
      } else {
        check_number_whole(value)
        private$.loan_term = value
      }
    }
  ),
  private = list(
    .purchase_price = numeric(),
    .down_payment = numeric(),
    .interest_rate = numeric(),
    .loan_term = numeric(),

    calculate_amort_table = function() {
      monthly_interest <- annual_rate / 12
      num_payments <- years * 12
      monthly_payment <- self$monthly_payment

      payment_number <- 1:num_payments
      payment <- rep(monthly_payment, num_payments)
      interest <- numeric(num_payments)
      principal <- numeric(num_payments)
      balance <- numeric(num_payments)

      remaining_balance <- self$purchase_price - self$down_payment

      for (t in 1:n) {
        interest[t] <- round(remaining_balance * monthly_interest, 2)
        principal[t] <- payment[t] - interest[t]
        remaining_balance <- remaining_balance - principal[t]
        balance[t] <- remaining_balance
      }

      amortization_table <- data.frame(
        Month = payment_number,
        Payment = round(payment, 2),
        Interest = round(interest, 2),
        Principal = round(principal, 2),
        Balance = round(pmax(balance, 0), 2)
      )

      self$amortization_table <- amortization_table

      invisible(self)
    },

    calculate_payment = function() {
      num_payments <- self$loan_term * 12
      monthly_interest <- self$interest_rate / 12
      monthly_payment <- .calculate_monthly_payment(
        P = self$purchase_price - self$down_payment,
        i = monthly_interest,
        n = num_payments
      )
      self$monthly_payment <- monthly_payment

      invisible(self)
    }
  ),
  public = list(
    amortization_table = data.frame(),
    monthly_payment = numeric(),
    initialize = function(purchase_price,
                          down_payment,
                          interest_rate,
                          loan_term) {
      self$purchase_price <- purchase_price
      self$down_payment <- down_payment
      self$interest_rate <- interest_rate
      self$loan_term <- loan_term

      private$calculate_payment()
      private$calculate_amort_table()

      invisible(self)
    }
  )
)

.calculate_monthly_payment <- function(P, i, n) {
  A <- (i * P * (1 + i)^n) / ((1 + i)^n - 1)
  return(A)
}
