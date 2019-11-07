# Creates or reads existing user-specific .Renviron file
user.renviron = path.expand(file.path("~", ".Renviron")) 

# Opens user-specific .Renviron file
file.edit(user.renviron)
