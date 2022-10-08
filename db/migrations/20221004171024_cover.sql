-- migrate:up

alter table source
    add cover text;

-- migrate:down

alter table source
    drop cover;
