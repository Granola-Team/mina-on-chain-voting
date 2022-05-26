const db = require('../config/db.config.js');
const UserStatistics = db.UserStatistics;

/**
 * Save a UserStatistics object to database MySQL/PostgreSQL
 * @param {*} req 
 * @param {*} res 
 */
exports.createUserStatistics = (req, res) => {
    let userStatistics = {};

    try{
        // Building UserStatistics object from upoading request's body
        userStatistics.user_name = req.body.user_name;
        userStatistics.user_key = req.body.user_key;
        userStatistics.hardfork = req.body.hardfork;
     
    
        // Save to MySQL database
        UserStatistics.create(userStatistics, 
                          {attributes: ['user_id', 'user_name', 'user_key', 'hardfork']})
                    .then(result => {    
                      res.status(200).json(result);
                    });
    }catch(error){
        res.status(500).json({
            message: "Fail!",
            error: error.message
        });
    }
}

/**
 * Retrieve UserStatistics information from database
 * @param {*} req 
 * @param {*} res 
 */
exports.userStatistics = (req, res) => {
    // find all UserStatistics information from 
    try{
        UserStatistics.findAll({attributes: ['user_id', 'user_name', 'user_key', 'hardfork']})
        .then(userstatistics => {
            res.status(200).json(userstatistics);
        })
    }catch(error) {
        // log on console
        console.log(error);

        res.status(500).json({
            message: "Error!",
            error: error
        });
    }
}

exports.getUsersStatistic = (req, res) => {
    UserStatistics.findByPk(req.params.id, 
                        {attributes: ['user_id', 'user_name', 'user_key', 'hardfork']})
        .then(userstatistics => {
          res.status(200).json(userstatistics);
        }).catch(error => {
          // log on console
          console.log(error);

          res.status(500).json({
              message: "Error!",
              error: error
          });
        })
}

/**
 * Updating a UserStatistics
 * @param {*} req 
 * @param {*} res 
 */
exports.updateUsersStatistic = async (req, res) => {
    try{
        let user_statistics = await UserStatistics.findByPk(req.body.user_id);
    
        if(!user_statistics){
            // return a response to client
            res.status(404).json({
                message: "Not Found for updating a User Statistics with id = " + req.body.user_id,
                error: "404"
            });
        } else {    
            // update new change to database
            let updatedObject = {
                user_name: req.body.user_name,
                user_key: req.body.user_key,
                hardfork: req.body.hardfork
            }
            let result = await UserStatistics.update(updatedObject,
                              { 
                                returning: true, 
                                where: {user_id: req.body.user_id},
                                attributes: ['user_id', 'user_name', 'user_key', 'hardfork']
                              }
                            );

            // return the response to client
            if(!result) {
                res.status(500).json({
                    message: "Error -> Can not update a user statistics with id = " + req.params.user_id,
                    error: "Can NOT Updated",
                });
            }

            res.status(200).json(result);
        }
    } catch(error){
        res.status(500).json({
            message: "Error -> Can not update a User Statistics with id = " + req.params.user_id,
            error: error.message
        });
    }
}

/**
 *  Delete a UserStatistics by ID
 * @param {*} req 
 * @param {*} res 
 */
exports.deleteUserStatistics = async (req, res) => {
    try{
        let user_id = req.params.user_id;
        let user_statistics = await UserStatistics.findByPk(user_id);

        if(!user_statistics){
            res.status(404).json({
                message: "Does Not exist a User Statistics with id = " + user_id,
                error: "404",
            });
        } else {
            await UserStatistics.destroy();
            res.status(200);
        }
    } catch(error) {
        res.status(500).json({
            message: "Error -> Can NOT delete a customer with id = " + req.params.user_id,
            error: error.message
        });
    }
}
