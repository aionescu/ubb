import React, {useEffect, useState} from 'react'
import styles from './HomePage.module.css'
import ModalLogin from "../ModalLogin/ModalLogin";
import ModalRegister from "../ModalRegister/ModalRegister";
import ApiCalls from "../Api/ApiCalls";
import {useNavigate} from "react-router-dom";

const HomePage = () => {
    let navigate = useNavigate()

    const [showLoginModal, setShowLoginModal] = useState(false);
    const [showRegisterModal, setShowRegisterModal] = useState(false);


    function getAllFiles() {

        if (ApiCalls.generateTokenHeader(localStorage.getItem('token') !== undefined)) {
            console.log("Getting all files")
            console.log('Authorization: ' + ApiCalls.generateTokenHeader(localStorage.getItem('token')))

            fetch(ApiCalls.host + "files", {
                method: 'GET',
                headers: {
                    'Authorization': ApiCalls.generateTokenHeader(localStorage.getItem('token'))
                }
            }).then(result => {
                if (result.status === 200) {
                    navigate('/myfiles')
                    console.log("User is logged in")
                } else {
                    setShowLoginModal(true)
                }
            })
        }

    }

    function goToGuestPage() {
        navigate('/myfiles')
    }

    function isLoggedIn() {
        if (localStorage.getItem("token")) {
            return false
        }
        return true
    }

    useEffect(() => {
    }, [showRegisterModal])
    return (
        <article className={styles.container}>
            <h1 className={styles.title}>ByteShare</h1>
            <h2 className={styles.subtitle}>Online editor</h2>
            <button className={styles.get_started_button} onClick={() => getAllFiles()}>Get Started</button>
            <button className={styles.enter_as_guest_button} onClick={() => goToGuestPage()}
                    hidden={!isLoggedIn()}>Enter as guest
            </button>
            <ModalLogin show={showLoginModal}
                        close={() => setShowLoginModal(false)}
                        openRegister={() => {
                            setShowRegisterModal(true);
                            setShowLoginModal(false)
                        }}/>
            <ModalRegister show={showRegisterModal} close={() => setShowRegisterModal(false)}/>
        </article>
    )
}
export default HomePage