Database
========

* sudo pacman -S postgres
* [opt] change postgres user home to /home/postgres
* [opt] add a .bash_profile with "export PGDATA = /var/lib/postgres/data"
* [opt] you can use su - postgres to have a login shell
* sudo -i -u postgres
* (as postgres) initdb
* (as postgres) create user --interactive
* (as postgres) createdb -O jules jules
* (as jules) psql -d jules -U jules
	CREATE TABLE event (
		bigserial primary key,
        jid  text NOT NULL,
        username text NOT NULL,
        stamp timestamptz NOT NULL,
        tgt   text NOT NULL,
     	minions text[],
     	fun     text NOT NULL,
    	arg     text[])
