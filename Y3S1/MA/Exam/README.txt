Todo

To manage current tasks, a firm designed a client-server system.
The server provides a list of tasks, each task having the following props
  id - integer,
  text - string,
  status - string with values 'active' or 'done',
  version - integer incremented by the server whenever the task is updated.
Develop a mobile application (client) as follows.

1. An input field is shown, allowing the user to search for tasks [0.5p].
When the value of this input contains at least one character [0.5p], the app fetches the tasks
containing that value via GET /task?q=value [1.5p].

2. A list shows the tasks fetched at 1 [1.5p]. For each task, its text is shown, followed by
a button: 'Close' (if status is 'active'), or 'Reopen' (if status is 'done') [1p].

3. When the user triggers 'Close' or 'Reopen' for a given task [1p], the app will update the
task via PUT /task/:id, including in the request body the task received from the server,
but with the status 'done', respectively 'active' [1p].

4. If the PUT operation fails with 409 (version conflict), after the app informs the user
about that version conflict, it will re-fetch the list [0.5p], allowing the user to retry the 'Close'
or 'Reopen' operations [0.5p].

5. Whenever the app performs HTTP operations, it shows a progress indicator [0.5p].
Any IO errors will be reported to the user [0.5p].
