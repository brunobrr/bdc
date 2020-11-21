# # Configurar o git em um computador

# install.packages("usethis")
# library(usethis)

# ### Configurando o Git
# usethis::use_git_config(user.name = "Bruno R. Ribeiro", # Seu nome
#                         user.email = "ribeiro.brr@gmail.com") # Seu email

# ### Configurando o GitHub + RStudio
# # Criar um novo token no GitHub:
  
# usethis::browse_github_token()

# # Make sure to copy your new personal access token now. You won’t be able to see it again!
# # e0cb37502c3d2f058953e0ae1f62b7cd52d88f60 

# # Abra o arquivo .Renviron:
# usethis::edit_r_environ()

# # git status
# # git reset HEAD
# # get ignore


# ### Configuração por repositorio
# git remote add origin https://github.com/brunobrr/risk_assessment_flora_Brazil.git
# git branch -M master

# git add . # Add todos os arquivos

# git add .gitignore # Adicionar apenas um arquivo
# git commit -m "Adicionando gitignore"
# git status
# # Para sair do vim digitar (esc e ":q", sem aspas)
# git push -u origin master # utilizar apenas a primeira vez. Depois é preciso utilizar apenas (git push)
# git push


######### Trabalhando com branch

# git branch novo_branch  # criar branch
# git checkout novo_branch  # mudar para o brach

# git add
# git commit -m "..."
# git push origin my branch


######## Materias para estudo
# Computatation skills for biology https://b-ok.lat/book/3698934/3ac99a
# https://www.datascienceatthecommandline.com/2e/




