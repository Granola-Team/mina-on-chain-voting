import React from 'react';
import { BrowserRouter as Router, Route, Routes } from 'react-router-dom';
import Home from './pages/Home';

const App = () => {
  return (
    <Router>
      <Routes>
        <Route path="/" element={<Home  testing={false} />} />
        <Route path="/:key" element={<Home  testing={false} />} />
        <Route path="/:key/testing" element={<Home testing={true}/>}/>
        <Route path="/testing" element={ <Home testing={true} /> } />
      </Routes>
    </Router>
  );
};

export default App;
