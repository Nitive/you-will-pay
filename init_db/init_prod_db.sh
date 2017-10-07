cd `dirname $0`

# create user (skip if exist)
createuser ywp_user || true

# create db (skip if exist)
createdb ywp_db || true

# create tables
psql -f create_shape.sql ywp_db
