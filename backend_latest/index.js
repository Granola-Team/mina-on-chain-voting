const { Client } = require('pg');
const express = require('express');
var app = express();
const bodyparser = require('body-parser');
const cors = require('cors');

app.use(bodyparser.json());
app.use(cors());

const client = new Client({
    user: "postgres",
    password: "postgres",
    host: "localhost",
    port: 5432,
    database: "mina_fork"
})

client.connect((err) => {
    if(!err)
    console.log("Db connection successful");
    else
    console.log("Db connections fails \n Error: " + Json.stringify(err, undefined, 2));
});

app.listen(8000, () => console.log('Exress server up'));

//get all accounts
app.get('/accounts', (req,res) => {
    client.query('SELECT * FROM accountinfo', (err, response) => {
        if(!err)
        res.send(response.rows);
        else
        console.log(err);   
    })
});

//get all users
app.get('/authors', (req,res) => {
    client.query('SELECT * FROM author', (err, response) => {
        if(!err)
        res.send(response.rows);
        else
        console.log(err);   
    })
});

//get a user 
app.get('/users/:id', (req,res) => {
    const id = req.params.id;
    client.query(`SELECT * FROM users WHERE userID = ${id}`, (err, response) => {
        if(!err)
        res.send(response.rows);
        else
        console.log(err);   
    })
});

//add a new user
app.post('/users', (req,res) => {
    const { userID, name, email} = req.body;
    console.log(req.body);
    client.query(`INSERT INTO users (userID, name, email) VALUES (${userID}, '${name}', '${email}') `, (err,response) => {
        if(!err)
        res.send("User added Succesfully");
        else
        console.log(err);   

    });
});

//delete a user
app.delete('/users/:id', (req,res) => {
    const id = req.params.id;
    client.query(`DELETE FROM users WHERE userID = ${id}`, (err, response) => {
        if(!err)
        res.send("Deleted Succesfully");
        else
        console.log(err);   
    })
});

//update a user
app.put('/users/:id', (req,res) => {
    const id = req.params.id;
    
    const { userID, name, email} = req.body;
    console.log(req.body);
    client.query(`UPDATE users SET name = '${name}', email='${email}' `, (err, response) => {
        if(!err)
        res.send("User Updated Succesfully");
        else
        console.log(err);   
    })
});

