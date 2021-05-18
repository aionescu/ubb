select resource_type, request_mode, request_type, request_status, request_session_id
from sys.dm_tran_locks
where request_owner_type = 'TRANSACTION'
