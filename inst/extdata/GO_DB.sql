--
-- GO_DB schema
-- ============
--

CREATE TABLE go_ontology (
  ontology VARCHAR(9) PRIMARY KEY,
  term_type VARCHAR(18) UNIQUE NOT NULL
);
CREATE TABLE go_term (
  _id INTEGER PRIMARY KEY,
  go_id CHAR(10) UNIQUE NOT NULL,
  term VARCHAR(255) NOT NULL,
  ontology VARCHAR(9) NOT NULL REFERENCES go_ontology,
  definition TEXT NULL
);

CREATE TABLE go_bp_offspring (
  _id INTEGER NOT NULL REFERENCES go_term,
  offspring_id INTEGER NOT NULL REFERENCES go_term
);
CREATE TABLE go_bp_parents (
  _id INTEGER NOT NULL REFERENCES go_term,
  parent_id INTEGER NOT NULL REFERENCES go_term,
  relationship_type VARCHAR(7) NOT NULL
);
CREATE TABLE go_cc_offspring (
  _id INTEGER NOT NULL REFERENCES go_term,
  offspring_id INTEGER NOT NULL REFERENCES go_term
);
CREATE TABLE go_cc_parents (
  _id INTEGER NOT NULL REFERENCES go_term,
  parent_id INTEGER NOT NULL REFERENCES go_term,
  relationship_type VARCHAR(7) NOT NULL
);
CREATE TABLE go_mf_offspring (
  _id INTEGER NOT NULL REFERENCES go_term,
  offspring_id INTEGER NOT NULL REFERENCES go_term
);
CREATE TABLE go_mf_parents (
  _id INTEGER NOT NULL REFERENCES go_term,
  parent_id INTEGER NOT NULL REFERENCES go_term,
  relationship_type VARCHAR(7) NOT NULL
);
CREATE TABLE go_synonym (
  _id INTEGER REFERENCES go_term,
  synonym VARCHAR(255) NOT NULL,
  secondary CHAR(10) NULL,
  like_go_id INTEGER
);
CREATE TABLE go_obsolete (
  _id INTEGER PRIMARY KEY,
  go_id CHAR(10),
  term VARCHAR(255),
  ontology TEXT,
  definition TEXT
);

-- Metadata tables
CREATE TABLE metadata (
  name VARCHAR(80) PRIMARY KEY,
  value VARCHAR(255)
);
CREATE TABLE map_counts (
  map_name VARCHAR(80) PRIMARY KEY,
  count INTEGER NOT NULL
);
CREATE TABLE map_metadata (
  map_name VARCHAR(80) NOT NULL,
  source_name VARCHAR(80) NOT NULL,
  source_url VARCHAR(255) NOT NULL,
  source_date VARCHAR(20) NOT NULL
);

CREATE INDEX gs1 on go_synonym(_id);
CREATE INDEX gs2 on go_synonym(synonym);
CREATE INDEX gt0 on go_term(go_id);
CREATE INDEX gt1 on go_term(_id);
CREATE INDEX gt2 on go_obsolete(_id);
CREATE INDEX gt3 on go_term(ontology, go_id);
CREATE INDEX of1 on go_bp_offspring(_id, offspring_id);
CREATE INDEX of2 on go_bp_offspring(offspring_id, _id);
CREATE INDEX of3 on go_mf_offspring(_id, offspring_id);
CREATE INDEX of4 on go_mf_offspring(offspring_id, _id);
CREATE INDEX of5 on go_cc_offspring(_id, offspring_id);
CREATE INDEX of6 on go_cc_offspring(offspring_id, _id);
CREATE INDEX pa1 on go_bp_parents(_id, parent_id);
CREATE INDEX pa2 on go_bp_parents(parent_id, _id);
CREATE INDEX pa3 on go_mf_parents(_id, parent_id);
CREATE INDEX pa4 on go_mf_parents(parent_id, _id);
CREATE INDEX pa5 on go_cc_parents(_id, parent_id);
CREATE INDEX pa6 on go_cc_parents(parent_id, _id);

