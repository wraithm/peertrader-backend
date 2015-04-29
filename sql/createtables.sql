-- Insert this into the db with: psql -d peertrader -U postgres < createtables.sql

DROP TABLE peertrader_admins CASCADE;

CREATE TABLE peertrader_admins (
    login               text REFERENCES snap_auth_user(login) UNIQUE
);

-- INSERT INTO peertrader_admins VALUES ('admin@peertrader.com');
