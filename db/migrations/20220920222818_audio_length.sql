-- migrate:up

alter table source
    drop column if exists sample_range;


-- migrate:down

