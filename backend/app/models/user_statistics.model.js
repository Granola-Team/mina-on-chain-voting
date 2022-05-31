module.exports = (sequelize, Sequelize) => {
        const userStatistics = sequelize.define('users_statistics', { 
          user_id: {
            type: Sequelize.INTEGER,
            autoIncrement: true,
            primaryKey: true
          },
          user_name: {
                        type: Sequelize.STRING
          },
          user_key: {
                  type: Sequelize.STRING
        },
          hardfork: {
                        type: Sequelize. BOOLEAN, allowNull: false, defaultValue: true

          }
        });
        
        return userStatistics;
}
