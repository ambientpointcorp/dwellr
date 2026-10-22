#' A single-SKU warehouse with an append-only ledger
#'
#' The simulation model developed in Chapter 1. It maintains two mutable
#' numbers — the signed inventory level and the quantity on order — and appends
#' a row to a private ledger whenever either changes. Every other quantity is
#' derived from those two on demand, using the identities of Section 1.2:
#'
#' \deqn{I_t = I_t^{+} - I_t^{-}, \qquad IP_t = I_t + O_t.}
#'
#' Deriving state from a ledger rather than maintaining it alongside one is the
#' approach Part II applies to real operations, where the ledger is an ERP's
#' event stream rather than a list built in a loop.
#'
#' @section Timing convention:
#' A receipt is posted at `period`; an issue is posted at `period + 1`. The
#' offset encodes an ordering within the period — arrivals first, demand
#' afterward. This is an assumption, not a property of the data, and whether it
#' holds in a given operation depends on receiving hours and pick waves.
#'
#' @section Methods:
#' \describe{
#'   \item{`place(qty, period)`}{Record an order placed: position rises, level
#'     does not.}
#'   \item{`receive(qty, period)`}{Record a receipt: level rises, outstanding
#'     falls.}
#'   \item{`ship(qty, period)`}{Record an issue. The level may go negative,
#'     which records a backorder.}
#'   \item{`pickable()`}{\eqn{I_t^{+}}, the quantity available to pick.}
#'   \item{`owed()`}{\eqn{I_t^{-}}, the quantity owed.}
#'   \item{`position()`}{\eqn{IP_t = I_t + O_t}.}
#'   \item{`history()`}{The ledger as a data frame.}
#' }
#'
#' @examples
#' w <- Warehouse$new(100)
#' for (t in 0:19) w$ship(5, t)
#' tail(w$history(), 2)
#' @export
#' @importFrom R6 R6Class
# `R6::R6Class()` here is a top-level assignment, evaluated when the package is
# installed rather than inside any function body. R CMD check scans function
# bodies for `::` usage, so the qualification alone leaves R6 looking unused and
# raises "Namespace in Imports field not imported from". The explicit importFrom
# is what settles it.
Warehouse <- R6::R6Class("Warehouse",
  public = list(
    #' @field stock Signed inventory level: held, less owed.
    stock       = NULL,
    #' @field outstanding Quantity ordered and not yet received.
    outstanding = NULL,

    #' @description Create a warehouse.
    #' @param opening Opening inventory level.
    #' @param on_order Opening quantity on order.
    initialize = function(opening = 0, on_order = 0) {
      self$stock <- opening
      self$outstanding <- on_order
      private$ledger <- list()
      private$post(0, "opening", opening)
    },

    #' @description Record an order placed.
    #' @param qty Quantity ordered.
    #' @param period Period in which the order is placed.
    place = function(qty, period) {
      self$outstanding <- self$outstanding + qty
      private$post(period, "order placed", qty)
      invisible(self)
    },

    #' @description Record a receipt, at the start of a period.
    #' @param qty Quantity received.
    #' @param period Period in which the receipt lands.
    receive = function(qty, period) {
      self$outstanding <- max(0, self$outstanding - qty)
      self$stock <- self$stock + qty
      private$post(period, "receipt", qty)
      invisible(self)
    },

    #' @description Record an issue, at the end of a period.
    #' @param qty Quantity issued.
    #' @param period Period in which demand occurs.
    ship = function(qty, period) {
      self$stock <- self$stock - qty
      private$post(period + 1, "issue", -qty)
      invisible(self)
    },

    #' @description On hand inventory.
    pickable = function() max(0,  self$stock),
    #' @description Backorders.
    owed     = function() max(0, -self$stock),
    #' @description Inventory position.
    position = function() self$stock + self$outstanding,

    #' @description The ledger.
    history = function() {
      d <- do.call(rbind, lapply(private$ledger, as.data.frame))
      rownames(d) <- NULL
      d
    }
  ),
  private = list(
    ledger = NULL,
    post = function(period, event, qty) {
      private$ledger[[length(private$ledger) + 1L]] <-
        list(period = period, event = event, qty = qty,
             level    = self$stock,
             pickable = max(0,  self$stock),
             owed     = max(0, -self$stock),
             on_order = self$outstanding,
             position = self$stock + self$outstanding)
    }
  )
)
