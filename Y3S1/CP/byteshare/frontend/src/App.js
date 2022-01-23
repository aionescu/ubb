import React, {useState} from 'react';
import './App.css';
import {Routes, Route, useNavigate} from 'react-router-dom';
import Header from "./Components/Header/Header";
import Environment from "./Components/Environment/Environment";
import Files from "./Components/Files/Files";
import HomePage from "./Components/HomePage/HomePage";
import ApiCalls from "./Components/Api/ApiCalls";
import GuestEnvironment from "./Components/GuestEnvironment/GuestEnvironment";


function checkIfLoggedIn() {

    if (localStorage.getItem("token")) {
        return <Files/>
    }
    return <GuestEnvironment/>

    // console.log("Getting all files")
    // console.log('Authorization: ' + ApiCalls.generateTokenHeader(localStorage.getItem('token')))
    //
    // fetch(ApiCalls.host + "files", {
    //     method: 'GET',
    //     headers: {
    //         'Authorization' : ApiCalls.generateTokenHeader(localStorage.getItem('token'))
    //     }
    // }).then(result => {
    //     if (result.status === 200) {
    //         setNavigate(<Files/>)
    //         console.log("USER IS LOGGED IN! NAVIGATE TO: /myfiles")
    //     } else {
    //         console.log("USER IS NOT LOGGED!")
    //     }
    // })
}

function App() {

    return (
        <React.Fragment>
            <Routes>
                <Route path={'/'} element={<HomePage/>}/>
                <Route path={'/myfiles'} element={checkIfLoggedIn()}/>
                <Route path={'/guest'} element={checkIfLoggedIn()}/>
            </Routes>
        </React.Fragment>

    );
}

export default App;
