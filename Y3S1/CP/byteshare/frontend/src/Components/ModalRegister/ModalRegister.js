import React, {useState} from 'react'
import ModalTemplate from "../ModalTemplate/ModalTemplate";
import {useNavigate} from "react-router-dom";
import ApiCalls from "../Api/ApiCalls";

const ModalRegister = (props) => {
    let navigate = useNavigate()

    const [username, setUsername] = useState('');
    const [password, setPassword] = useState('');
    const [confirmedPassword, setConfirmedPassword] = useState('');

    function register(username, password, confirmedPassword) {
        if (password !== confirmedPassword) {
            //TODO: validation error
        }

        fetch(ApiCalls.host + "register", {
            method: 'POST',
            headers: {'Content-Type' : 'application/json'},
            body: JSON.stringify({"username": username, "password": password})
        }).then(result => {
            if (result.status === 201) {
                console.log("SUCCESSFUL REGISTER!")
                navigate('/myfiles')
                result.json().then(res => localStorage.setItem("token", res.token))
            } else {
                console.log("FAILED REGISTER!")
                //TODO: Add error for invalid register
            }
        })
    }

    return (
        <ModalTemplate show={props.show} onClose={props.close}>
            <h1>Create Account</h1>
            <input type={"text"} placeholder={"Username"} required={true} value={username}
                   onInput={event => setUsername(event.target.value)}/>
            <input type={"email"} placeholder={"E-mail"} required={true}/>
            <input type={"password"} placeholder={"Choose Password"} required={true} value={password}
                   onInput={event => setPassword(event.target.value)}/>
            <input type={"password"} placeholder={"Confirm Password"} required={true} value={confirmedPassword}
                   onInput={event => setConfirmedPassword(event.target.value)}/>
            <button onClick={() => register(username, password, confirmedPassword)}>Register</button>
        </ModalTemplate>
    )
}
export default ModalRegister