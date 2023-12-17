-- migrate:up

alter table melo.collection
    add library boolean default false not null;


-- migrate:down

alter table melo.collection
    drop column library;
