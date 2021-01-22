
Installation:


1. To install:

```
  install.packages( "devtools", ask=F, dependencies=TRUE ) # to inter-operate with github
devtools::install_github( "brent0/Lobster.Archive", INSTALL_opts = "--no-multiarch")# to bootstrap by installing directly from github
```


2. Then, you need to have an Rprofile set up properly. 

```.

# store your passwords and login here and make sure they are secure
passwords = file.path( homedir, ".passwords" )
if (file.exists(passwords)) source( passwords )
```

3. Usage. ON FIRST RUN CLICK ALLOW ON SECURTITY PROMPTS!!!!

```.

 library("Lobster.Archive")
 Archive_App()
 ```
 
