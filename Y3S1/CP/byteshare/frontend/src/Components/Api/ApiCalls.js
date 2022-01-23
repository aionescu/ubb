import React, {Component} from 'react'


class ApiCalls {
    static host = "http://localhost:5000/"

    static generateUserAndPassAuthHeader(username, password) {
        return 'Basic ' + Buffer.from(username + ":" + password).toString('base64')
    }

    static generateTokenHeader(token) {
        return 'Bearer ' + token
    }
}
export default ApiCalls