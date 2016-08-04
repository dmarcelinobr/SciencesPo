library('shiny')
library('SciencesPo') # Color blind paletter

shinyServer(function(input, output) {

  output$dist_plot = renderPlot({

    y_data      = input$'y_data'
    data_sigma  = input$'data_sigma'
    prior_mu    = input$'prior_mu'
    prior_sigma = input$'prior_sigma'

    if (data_sigma <= 0L | prior_sigma <= 0L) return(NULL)

    n = length(y_data)
    data_mu = mean(y_data)
    post_mu = ((prior_mu/prior_sigma^2) + ((n * data_mu)/data_sigma^2))/
      ((1/prior_sigma^2) + (n/data_sigma^2))
    post_sigma = sqrt(1/((1/prior_sigma^2) + (n/data_sigma^2)))

    set.seed(42)
    y = seq(post_mu - 8 * post_sigma,
            post_mu + 8 * post_sigma,
            length.out = 500)  # to center plot on posterior
    y_prior = dnorm(y, prior_mu, prior_sigma)
    y_lik   = dnorm(y, data_mu,  data_sigma)
    y_post  = dnorm(y, post_mu,  post_sigma)

    y_max = max(c(y_prior, y_lik, y_post))

    pal = rev(SciencesPo::pub_pal("scipo")(3))

    plot(y, y_prior, type = 'l', col = pal[1],
         lty = 2, xlim = c(min(y), max(y)), ylim = c(0, y_max),
         ylab = 'density', lwd = 2)
    lines(y, y_lik,  type = 'l', col = pal[2], lwd = 2)
    lines(y, y_post, type = 'l', col = pal[3], lwd = 2)
    abline(v = y_data, col = pal[2], lty = 3, lwd = 2)

    legend('topright', col = c(pal, pal[2]),
           lty = c(2, 1, 1, 3), cex = 1.5, lwd = 2, bty = 'n',
           legend = c('Prior', 'Likelihood', 'Posterior', 'Data'))

  })

})
