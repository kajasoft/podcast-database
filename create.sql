create table feeds (
  feed_id serial primary key,
  feed_url varchar not null unique,
  feed_title varchar,
  feed_link varchar,
  feed_itunes_url varchar,
  feed_description text,
  feed_last_build_date varchar, -- for now
  feed_explicit bool null, 
  feed_keywords varchar,
  feed_categories varchar,
  feed_summary text  -- may be redundnat with description
  );

drop table items;
create table items (
  item_id serial primary key,
  feed_id integer not null references feeds (feed_id),
  item_title varchar not null,
  item_link varchar,
  item_summary text,
  item_pubdate text, 
  item_guid varchar not null,
  item_categories varchar,
  item_audio_url varchar not null,
  item_duration varchar,
  item_explicit boolean null
  );

alter table items add constraint items_feed_id_guid_uniq_key UNIQUE (feed_id, item_guid); 


