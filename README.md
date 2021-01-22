To install

```
install.packages( "devtools", ask=F, dependencies=TRUE ) # to inter-operate with github
devtools::install_github( "brent0/Lobster.Archive", INSTALL_opts = "--no-multiarch")# to bootstrap by installing directly from github
```

Rprofile 

```
# Add code to your Rprofile.site file that sources the needed oracle usernames, passwords ans server
oracle.password = "yourpassword"
oracle.username = "yourusername"
oracle.server = "yourserver"
```

Usage.
ON FIRST RUN CLICK ALLOW ON SECURTITY PROMPTS!!!!

```
 library("Lobster.Archive")
 Archive_App()
 ```
 
