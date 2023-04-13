CREATE TABLE IF NOT EXISTS dists (
  ID integer PRIMARY KEY,
  name varchar(128) NOT NULL UNIQUE,
  url text NOT NULL);

CREATE TABLE IF NOT EXISTS dist_projects (
  ID integer PRIMARY KEY,
  dist integer NOT NULL,
  project integer NOT NULL,
  FOREIGN KEY(dist) REFERENCES dists(ID),
  FOREIGN KEY(project) REFERENCES projects(ID),
  UNIQUE(dist, project));

CREATE TABLE IF NOT EXISTS dist_excluded_paths (
  ID integer PRIMARY KEY,
  dist integer NOT NULL,
  path TEXT NOT NULL,
  FOREIGN KEY(dist) REFERENCES dists(ID));

CREATE TABLE IF NOT EXISTS dist_releases (
  ID integer PRIMARY KEY,
  dist integer NOT NULL,
  version varchar(128) NOT NULL,
  timestamp bigint NOT NULL,
  FOREIGN KEY(dist) REFERENCES dists(ID),
  UNIQUE(dist, version));

CREATE TABLE IF NOT EXISTS dist_release_projects (
  ID integer PRIMARY KEY,
  dist_release integer NOT NULL,
  project_release integer NOT NULL,
  FOREIGN KEY(dist_release) REFERENCES dist_releases(ID),
  FOREIGN KEY(project_release) REFERENCES project_releases(ID),
  UNIQUE(dist_release, project_release));

CREATE TABLE IF NOT EXISTS projects (
  ID integer PRIMARY KEY,
  name varchar(128) NOT NULL UNIQUE,
  source_directory text NOT NULL,
  disabled boolean NOT NULL);

CREATE TABLE IF NOT EXISTS project_sources (
  ID integer PRIMARY KEY,
  project integer NOT NULL,
  type varchar(64) NOT NULL,
  url TEXT NOT NULL,
  initargs TEXT NOT NULL,
  FOREIGN KEY(project) REFERENCES projects(ID));

CREATE TABLE IF NOT EXISTS project_excluded_systems (
  ID integer PRIMARY KEY,
  project integer NOT NULL,
  system varchar(128) NOT NULL,
  FOREIGN KEY(project) REFERENCES projects(ID),
  UNIQUE(project, system));

CREATE TABLE IF NOT EXISTS project_excluded_paths (
  ID integer PRIMARY KEY,
  project integer NOT NULL,
  path text NOT NULL,
  FOREIGN KEY(project) REFERENCES projects(ID));

CREATE TABLE IF NOT EXISTS project_releases (
  ID integer PRIMARY KEY,
  project integer NOT NULL,
  version varchar(128) NOT NULL,
  archive_md5 character(32) NOT NULL,
  source_sha1 character(40) NOT NULL,
  FOREIGN KEY(project) REFERENCES projects(ID),
  UNIQUE(project, version));

CREATE TABLE IF NOT EXISTS project_release_source_files (
  ID integer PRIMARY KEY,
  project_release integer NOT NULL,
  path text NOT NULL,
  FOREIGN KEY(project_release) REFERENCES project_releases(ID));

CREATE TABLE IF NOT EXISTS project_release_systems (
  ID integer PRIMARY KEY,
  project_release integer NOT NULL,
  name varchar(128) NOT NULL,
  file text NOT NULL,
  FOREIGN KEY(project_release) REFERENCES project_releases(ID),
  UNIQUE(project_release, name));

CREATE TABLE IF NOT EXISTS project_release_system_dependencies (
  ID integer PRIMARY KEY,
  system integer NOT NULL,
  dependency varchar(128) NOT NULL,
  FOREIGN KEY(system) REFERENCES project_release_systems(ID),
  UNIQUE(system, dependency));
