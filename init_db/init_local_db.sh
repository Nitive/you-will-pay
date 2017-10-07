cd `dirname $0`

# create user (remove if exist)
dropuser ywp_user || true
createuser ywp_user

# create db (remove if exist)
dropdb ywp_db || true
createdb ywp_db

# create tables and insert data
psql -f create_shape.sql ywp_db
psql -f create_data.sql ywp_db
