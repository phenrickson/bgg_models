# knitr chunk options

set_knitr_options = function() {
        
        knitr::opts_chunk$set(echo = F,
                              error = F,
                              warning = F,
                              dev="png",
                              fig.align = 'center',
                              out.width = '80%')
        
        # options for displaying
        options(knitr.duplicate.label = "allow",
                scipen = 999)
        
}

