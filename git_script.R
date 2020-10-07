install.packages("usethis")
library(usethis)

### Configurando o Git
usethis::use_git_config(user.name = "Bruno R. Ribeiro", # Seu nome
                        user.email = "ribeiro.brr@gmail.com") # Seu email

### Configurando o GitHub + RStudio
# Criar um novo token no GitHub:
  
usethis::browse_github_token()

# Make sure to copy your new personal access token now. You won’t be able to see it again!
# e0cb37502c3d2f058953e0ae1f62b7cd52d88f60 

# Abra o arquivo .Renviron:
usethis::edit_r_environ()
