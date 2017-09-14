# create db (remove if exist)
dropdb you-will-pay || true
createdb you-will-pay

# create tables and insert data
psql -f init_local_db/create_data.sql you-will-pay
