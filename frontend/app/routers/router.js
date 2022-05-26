let express = require('express');
let router = express.Router();
 
const customers = require('../controllers/controller.js');

const statistics = require('../controllers/statistics_controller.js');

router.post('/api/customer', customers.createCustomer);
router.get('/api/customer/:id', customers.getCustomer);
router.get('/api/customers', customers.customers);
router.put('/api/customer', customers.updateCustomer);
router.delete('/api/customer/:id', customers.deleteCustomer);

router.post('/api/statistics', statistics.createUserStatistics);
router.get('/api/statistics/:id', statistics.getUserStatistics);
router.get('/api/statistics', statistics.UserStatistics);
router.put('/api/statistic', statistics.updateUserStatistics);
router.delete('/api/statistic/:id', statistics.deleteUserStatistics);


module.exports = router;
