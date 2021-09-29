library(splines)
lq.loss <- function(y, y.hat, q = 1) {(abs(y - y.hat))^q}
get.errs <- function(test.set = NULL,
                     discarded = NULL,
                     q = 1) {
  sml.glm <- glm(arrivals ~
                   bs(hour, degree = 4)
                 + weekend
                 + as.factor(id),
                 data = arrivals.sub[-c(discarded, test.set), ],
                 family = "poisson")
  med.glm <- glm(arrivals ~
                   bs(hour, degree = 4)*weekend
                 + as.factor(id),
                 data = arrivals.sub[-c(discarded, test.set), ],
                 family = "poisson")
  big.glm <- glm(arrivals ~
                   bs(hour, degree = 4)*weekend
                 + bs(hour, degree = 4)*as.factor(id),
                 data = arrivals.sub[-c(discarded, test.set), ],
                 family = "poisson")
  sml.err <- mean(lq.loss(predict(object = sml.glm,
                                  newdata = arrivals.sub[test.set, -7],
                                  type = "response"),
                          arrivals.sub[test.set, 7],
                          q = q))
  med.err <- mean(lq.loss(predict(object = med.glm,
                                  newdata = arrivals.sub[test.set, -7],
                                  type = "response"),
                          arrivals.sub[test.set, 7],
                          q = q))
  big.err <- mean(lq.loss(predict(object = big.glm,
                                  newdata = arrivals.sub[test.set, -7],
                                  type = "response"),
                          arrivals.sub[test.set, 7],
                          q = q))
  return(c(sml.err, med.err, big.err))
}
