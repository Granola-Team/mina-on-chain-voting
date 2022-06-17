# onchain-signalling
Onchain Signalling for Mina Protocol - Monitors blocks and its transactions using memo variables.
Folders added represent each component needed for this project 
# Backend
This folder represents sequelize ORM to fetch data from database and it contains code which refers to mina_fork database.
But there is a problem this doesn't work without Sequelize ORM.
# Backend_latest
This folder represents backend code that refers sample postgresql database and its tables without using Sequelize ORM.
Previous Backend folder needs to be replaced by this framework inorder to make backend restapi and independent of Sequelize ORM.
# Frontend
This folder has UI components in react displaying datatable.
# Graphql
This folder represents Graphql queries that queries memo field of mina blockchain with 'magenta'value.
# Scheduler
This folder contains code to run in background which is background services.
# Terraform
This folder contains code related to automation script.
