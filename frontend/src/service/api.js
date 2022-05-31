import axios from 'axios';

const api = axios.create({
    baseURL: "http://ec2-3-99-167-141.ca-central-1.compute.amazonaws.com:8080/api",
    headers: {
        'Access-Control-Allow-Origin':'http://ec2-3-99-167-141.ca-central-1.compute.amazonaws.com:8080/',
        'Access-Control-Allow-Methods':'GET, POST, OPTIONS, PUT, PATCH, DELETE',
        'Access-Control-Allow-Headers':'X-Requested-With,content-type',
        'Access-Control-Allow-Credentials':true,  
    },
});

export default api;

