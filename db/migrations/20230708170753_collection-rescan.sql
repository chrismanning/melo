-- migrate:up
alter table collection add column rescan bool default true;

-- migrate:down
alter table collection drop column rescan;
