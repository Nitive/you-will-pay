INSERT INTO rooms (title, color)
VALUES ('Matrix', 'blue');

SELECT * FROM rooms;
INSERT INTO users (nickname, color)
VALUES
  ('Neo', 'black'),
  ('Trinity', 'white');

SELECT * FROM users;

INSERT INTO transactions (created, price, room_id, summary, user_id)
VALUES
  ('2017-09-14 13:00:00+03', 100, 1, 'Купил пистолеты на рынке', 1),
  ('2017-09-14 14:00:00+05', 200, 1, 'Программа обучения пилотирования вертолётом', 2);

SELECT * FROM transactions;
