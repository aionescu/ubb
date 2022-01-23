import React from 'react'
import styles from './Header.module.css'
import {useNavigate} from "react-router-dom";

const Header = () => {
    let navigate = useNavigate()

    function logout() {
        navigate('/')

        setTimeout("", 1000)
        localStorage.removeItem("token")
        localStorage.clear()
    }

    function goToHomePage() {
        navigate('/')
    }

    function isLoggedIn() {
        if (localStorage.getItem("token")) {
            return false
        }
        return true
    }

    return(
        <header className={styles.header}>
            <h1 className={styles.title} onClick={() => goToHomePage()}>ByteShare</h1>
            <h5 className={styles.logoutButton} onClick={() => logout()} hidden={isLoggedIn()}>Log out</h5>
        </header>
    )
}

export default Header