CREATE DATABASE mina_fork;    
CREATE TABLE account_info (id SERIAL PRIMARY KEY, account VARCHAR, voting bool, transaction_data TIMESTAMP);
