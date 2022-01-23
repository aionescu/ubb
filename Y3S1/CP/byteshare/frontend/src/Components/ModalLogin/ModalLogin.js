import React, {useState} from 'react'
import {useNavigate} from 'react-router-dom'
import ModalTemplate from "../ModalTemplate/ModalTemplate";
import ApiCalls from "../Api/ApiCalls";

const ModalLogin = (props) => {
    let navigate = useNavigate()
    const [username, setUsername] = useState('');
    const [password, setPassword] = useState('');

    function login(username, password) {

        console.log(username + " | " + password)
        console.log('Basic ' + Buffer.from(username + ":" + password).toString('base64'))

        fetch(ApiCalls.host + "token", {
            method: 'GET',
            headers: {
                'Authorization': ApiCalls.generateUserAndPassAuthHeader(username, password)
            }
        }).then(result => {
            if (result.status === 200) {
                console.log("SUCCESSFUL LOGIN!")
                navigate('/myfiles')
                result.json().then(res => localStorage.setItem("token", res.token))
            } else {
                console.log("FAILED LOGIN")
                //TODO: Add error for invalid login
            }
        })
    }

    return (
        <ModalTemplate show={props.show} onClose={props.close}>
            <h1>Log In</h1>
            <input type={"text"} placeholder={"Username"} value={username}
                   onInput={event => setUsername(event.target.value)}/>
            <input type={"password"} placeholder={"Password"} value={password}
                   onInput={event => setPassword(event.target.value)}/>
            <button onClick={() => login(username, password)}>Log In</button>
            <p>Dont have an account?
                <span
                    style={{'color': 'var(--main-fg-color-light)', 'fontWeight': 'bold', 'cursor': 'pointer'}}
                    onClick={() => props.openRegister()}>
                     Create one
                </span>
            </p>
        </ModalTemplate>
    )
}
export default ModalLogin