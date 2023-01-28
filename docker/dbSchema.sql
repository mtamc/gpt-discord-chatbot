-- PostgreSQL database dump
SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;
SET default_tablespace = '';
SET default_table_access_method = heap;

CREATE TABLE public.messages (
    db_id SERIAL PRIMARY KEY,
    tstamp TIMESTAMP NOT NULL DEFAULT now(),
    discord_msg_id VARCHAR ( 50 ) UNIQUE NOT NULL,
    discord_op_msg_id VARCHAR ( 50 ) NOT NULL,
    interlocutor_id VARCHAR ( 50 ) NOT NULL,
    personality VARCHAR ( 50 ) NOT NULL,
    content TEXT NOT NULL
);


ALTER TABLE public.messages OWNER TO desu;
