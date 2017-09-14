CREATE TABLE rooms (
  color VARCHAR(256) NOT NULL,
  id SERIAL NOT NULL, PRIMARY KEY (id),
  title VARCHAR(256) NOT NULL
);

INSERT INTO rooms (title, color)
VALUES ('Matrix', 'blue');

SELECT * FROM rooms;


CREATE TABLE users (
  color VARCHAR(256) NOT NULL,
  id SERIAL NOT NULL, PRIMARY KEY (id),
  nickname VARCHAR(256) NOT NULL
);

INSERT INTO users (nickname, color)
VALUES
  ('Neo', 'black'),
  ('Trinity', 'white');

SELECT * FROM users;


CREATE TABLE transactions (
  created timestamptz NOT NULL,
  id SERIAL NOT NULL, PRIMARY KEY (id),
  price int NOT NULL,
  room_id int NOT NULL, FOREIGN KEY (room_id) REFERENCES rooms(id),
  summary VARCHAR(256) NOT NULL,
  user_id int NOT NULL, FOREIGN KEY (user_id) REFERENCES users(id)
);

INSERT INTO transactions (created, price, room_id, summary, user_id)
VALUES
  ('2017-09-14 13:00:00+03', 100, 1, 'Купил пистолеты на рынке', 1),
  ('2017-09-14 14:00:00+05', 200, 1, 'Программа обучения пилотирования вертолётом', 2);

SELECT * FROM transactions;
