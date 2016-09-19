PlotConfidenceCurve <- function(r, k, n1, n2, effect.size, var, m = 100, level  = 0.01, color="none", verbose = TRUE) {
  #' This function plots a confidence curve for the point estimate of the difference in performance between two classifiers.
  #' It is assumed that the difference follows Student's t-distribution with k*r-1 degrees of freedom.
  #'
  #' Args:
  #'   r: The number of repetitions of k-fold cross-validation.
  #'   k: The k in k-fold cross-validation.
  #'   n1: The number of cases in a training set
  #'   n2: The number of cases in a validation set
  #'   effect.size: The point estimate of the difference in performance
  #'   var: The variance of the effect size
  #'   m: The number of nested confidence curves to be computed. Default is 100. 
  #'   level: Alpha level of the widest confidence interval. Default is 0.01. 
  #'   color: The color of the area under the confidence curve. Default is "none".
  #'   verbose: If TRUE, prints the area under the confidence curve; if not, not. Default is TRUE.
  #'
  #' Returns:
  #'   A plot of the confidence curve for effect.size.
  #'   The area under the confidence curve (AUCC).
  #'
  #' THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, 
  #' EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES 
  #' OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND 
  #' NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT 
  #' HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, 
  #' WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, 
  #' OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR 
  #' OTHER DEALINGS IN THE SOFTWARE.
  #' -------------------------------------------------------------------------------------------------- #
  # Error handling
  if (m <= 10 ) {
    warning("The number of nested confidence intervals is too small; interpolation won't give a smooth curve for ",
         m, " intervals.")
  }
  # -------------------------------------------------------------------------------------------------- #
  # Initialize a matrix to store alpha and lower and upper bounds of the nested confidence intervals.   
  CI <- matrix(0, 100, 3)
  # Calculate standard deviation with variance correction by Nadeau and Bengio (2003).  
  SD <- sqrt((1 / (k * r) + n2 / n1) * var) 

  # Calculate m nested confidence intervals.
  for (i in 0:(m - 1)){
    alpha <- level + i / m
    T <- qt(1 - alpha / 2, (k * r - 1)) # T-value for k*r-1 degrees of freedom; qt() is the quantile function.
    UB <- effect.size + T * SD # Upper bound of confidence interval.
    LB <- effect.size - T * SD # Lower bound of confidence interval.
    CI[i + 1, 1] <- alpha
    CI[i + 1, 2] <- LB
    CI[i + 1, 3] <- UB
  }
  # -------------------------------------------------------------------------------------------------- #
  # Plot the confidence curve.
  par(mar = c(5, 4, 4, 5) + .1)
  plot(CI[1, 2], 0 , pch = "", xlim = c(-0.10, 0.50), ylim = c(0, 1), 
       las = 1, xlab = "True difference in performance", ylab = "p-value")
  lines(CI[, 2], CI[, 1])
  lines(CI[, 3], CI[, 1])
  axis(4, las = 1, at = c(1, 0.8, 0.6, 0.4, 0.2, 0), labels = c(0, 20, 40, 60, 80, 100))
  mtext("Confidence level [%]", side = 4, line = 3)
  # -------------------------------------------------------------------------------------------------- #
  # Fill the area under the curve.
  # Start with the left part.
  if (color != "none") {
    cord.x <- c(CI[, 2], CI[m, 2])
    cord.y <- c(CI[, 1], CI[1, 1])
    polygon(cord.x, cord.y, col=color)
    # Then do the right part.
    cord.x <- c(CI[, 3], CI[m, 3])
    cord.y <- c(CI[, 1], CI[1, 1])
    polygon(cord.x, cord.y, col=color)
  }
  # -------------------------------------------------------------------------------------------------- #
  # Mark interesting points.
  abline(h = 0.05) # Mark p-value = 0.05
  abline(v = 0) # Mark the null value delta = 0.
  axis(side = 1, at = round(effect.size, 3), lwd = 2, cex.axis = 0.8) # Add the value of effect.size to the x-axis.
  # -------------------------------------------------------------------------------------------------- #
  # Estimate the area under the curve.
  if (verbose){
    AUCC <- 4 / sqrt(2 * pi) * SD
    cat("AUCC = ", round(AUCC, 4), "\n", sep = "")
    #return(AUCC)
  }
}