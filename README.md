# BiGUL-configuration-adaptation
Self-adaptation for configuration files with BiGUL


  The transformations folder contains the following files:

  - BiGUL programs for Apache and Nginx web servers technologies, respectively called TransfoApache and TransfoNginx. They implement the BX between the configuration files and the abstract views.
  - Configuration files used as sources for both technologies, respectively apache.conf and nginx.conf
  - Views resulting from the application of the BX on those source configuration files, Apache_output and Nginx_output.
  - Additional files containing the default values for each directive in both technology, ApacheDefaultValues ad NginxDefaultValues. Those files are given as parameters to the BX.
  - Well-indented versions of a couple of Nginx source and view is also given for the reader's ease, X_Pretty_printed files.
  
  - The Typefiles folder contains the definitions of haskell types for our sources and view.
  - The src folder contains the BiGUL implementation files at the time of writing.
  - The Parser folder contains the haskell code for parsers and pretty printing allowing to transform configuration files in well-suited BiGUL sources and the other way around.


How to use:

  Dependancies:
  The user must have GHC installed.

  While in the Transformations folder, run this command:
  
  ghci -isrc -iParsers/(WS)_Parser -iParsers/TypesAndFunctions Transfo(WS).hs -XTemplateHaskell -fcontext-stack=500 -XFlexibleContexts
  
  replacing every instance of "(WS)" by either "Apache" or "Nginx".
  
  Once the program is loaded, the following commands are available:
  - extractConfig: performs the get direction and shows the resulting view in console. This does NOT actually update the current view.
  - putbackConfig: performs the putback and shows the resulting new configuration file in console. This does NOT actually update the current configuration file.
  - extractConfigToFile: performs the get and rewrites the view file.
  - putbackConfigToFile: performs the putback and rewrites the source configuration file.
  
  Warning: if changes are made to the view file, the program needs to be reloaded by running ":r" in ghc in order for these changes to be taken into account. This is due to the view file being imported when the BX program loads and not dynamically when running a command.
