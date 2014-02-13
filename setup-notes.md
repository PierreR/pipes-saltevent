Database
========

* sudo pacman -S postgres
* sudo -i -u postgres
* (as postgres) initdb
* (as postgres) create user --interactive
* (as postgres) createdb -O jules jules
* () create table event (
		bigserial primary key,
        jid  varchar(20) NOT NULL,
        user varchar(20) NOT NULL,
        _stamp varchar(20) NOT NULL,
        tgt   varchar(20) NOT NULL,
     	minions [varchar(30)],
     	fun     varchar(20) NOT NULL,
    	arg     [Int]
