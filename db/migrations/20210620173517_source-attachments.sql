-- migrate:up

create table attachment
(
	id uuid default uuid_generate_v4() not null,
	uri text not null,
	kind text not null
);

create unique index attachment_id_uindex
	on attachment (id);

create unique index attachment_uri_uindex
	on attachment (uri);

alter table attachment
	add constraint attachment_pk
		primary key (id);

create table source_attachment
(
	source_id uuid not null
		constraint source_attachment_source_id_fk
			references source
				on delete cascade,
	attachment_id uuid not null
		constraint source_attachment_attachment_id_fk
			references attachment (id)
				on delete cascade,
	constraint source_attachment_pk
		primary key (source_id, attachment_id)
);

create index source_attachment_attachment_id_index
	on source_attachment (attachment_id);

create index source_attachment_source_id_index
	on source_attachment (source_id);


-- migrate:down

drop index if exists source_attachment_source_id_index;
drop index if exists source_attachment_attachment_id_index;

drop table if exists source_attachment;

drop index if exists attachment_uri_uindex;
drop index if exists attachment_id_uindex;

drop table if exists attachment;
