# Backend Documentation

## Language & Libraries

The backend is written in Python, using the [Flask](https://flask.palletsprojects.com/) library for implementing the REST API, and [SQLAlchemy](https://www.sqlalchemy.org/) for database integration.

It is recommended to use Python version `3.9` or newer.

The required libraries are specified in the [`requirements.txt`](../requirements.txt) file, and can be installed by running `pip install -r requirements.txt`.

## Database

The database provider used is PostgreSQL. The database creation script can be found in the [`db/ByteShare.sql`](../db/ByteShare.sql) file.

It's meant to be run from the `pgAdmin` web interface *after* the database was created.

## REST API

The API consists of 2 main routes: `/files` and `/auth`. More information about the endpoints of each route can be found in their respective documentation sections.

## Authentication

Authentication in the app is handled using [JSON Web Tokens](https://jwt.io/) and HTTP Bearer-Token authentication.

## Source Code Organization

Database models are stored in the [`src/models`](../src/models) folder.

The REST API modules are stored in the [`src/apis`](../src/apis) folder.

App configuration and initialization is done in [`src/config.py`](../src/config.py), including SQL server configuration.

The app's entry point is [`src/main.py`](../src/main.py)

## Running the web server

The easiest way to run the web server is using the provided [`run.sh`](../run.sh) script, using the following command:

```sh
./run.sh
```

## Hot Reload & Debugging

By default, the server runs in *development* mode, and Hot Reload is enabled in order to facilitate a fast development feedback loop.

To run in *release* mode, set the `FLASK_ENV` environment to `release` prior to running, like so:

```sh
FLASK_ENV=release ./run.sh
```
