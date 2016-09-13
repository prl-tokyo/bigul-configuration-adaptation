Experiments
=================

Adaption Experiment
-------

~~~~~~
.
`-- adapt
    |-- myplot1.pdf             plot of response time
    |-- myplot2.pdf             plot of request number
    |-- notAdapt3.log           log of response time without self-adaptive server
    |-- adapt3.log              log of response time with self-adaptive server
    |-- apache2.conf            configuration file for apache server
    |-- nginx.conf              configuration file for nginx server
    |-- reqplot.jl              julia program for plotting the log
    `-- request.js              node.js program for generating the requests
~~~~~~

In this experiment, we generate a lot of requests to the web server with or without the self-adaptive layer:

* Without the self-adaptive layer:

  1. Use apache2.conf as the configuration
  2. Setup '/var/www/blog' and '/var/www/blog-low' as the site with normal quality and lower quality correspondently.
  3. Run request.js to generate requests.
  4. Use reqplot.jl to plot the log generated in the previous step.

* With the self-adaptive layer:

  1. In `Transformation/`, build the adapter by `ghc -isrc -iParsers/Apache_Parser -iParsers/Nginx_Parser -iParsers/TypesAndFunctions SelfAdapter.hs --make -XTemplateHaskell -fcontext-stack=500 -XFlexibleContexts -outputdir build`
  2. Run the adapter by `sudo ./SelfAdapter`
  3. The next steps are the same as without the self-adaptive layer.


Migration Experiment
--------

~~~~~
.
`-- migrate
    |-- apache.conf
    |-- nginx.conf
    `-- nginx.conf.out
~~~~

In this experiment, we show that the BX program between the concrete models and the abstract model can be used to transform from one concrete model to another.

The file 'apache.conf' is the source model. The file 'nginx.conf' is a simple template used as the target of the BX. To build the migration program, run `ghc -isrc -iParsers/Apache_Parser -iParsers/Nginx_Parser -iParsers/TypesAndFunctions migrate.hs --make -XTemplateHaskell -fcontext-stack=500 -XFlexibleContexts -outputdir build` and run `./migrate` at this directory. `nginx.conf.out` is the generated output.


Updating Experiment
--------
In this experiment, we show that another advantage of our approach is that we can easily migrate to the new version of the web server without rewrite the BX between the abstract and the concrete model at all. Instead, we can write a much simpler BX between the old concrete model and the new concrete model.

In `Transformation/TypeFiles/NginxNewTypes.hs`, we define a hypothetical new version of the nginx web server. The difference is that the ErrorLog option now also takes the files permission as the argument besides the file path. 

In `Transformation/TransfoNginxNewToOld.hs`, we define `transNginxOldToNew`, which transform the new to the old. And we compose it with the old BX between the old nginx server and the abstract.
