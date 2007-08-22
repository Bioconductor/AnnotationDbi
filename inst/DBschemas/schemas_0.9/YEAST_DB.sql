CREATE TABLE sgd (
  id INTEGER PRIMARY KEY,
  systematic_name VARCHAR(11) NULL UNIQUE,
  gene_name VARCHAR(12) NULL UNIQUE,
  sgd_id CHAR(10) NOT NULL UNIQUE,  -- check the UNIQUE assumption
);

CREATE TABLE chrlengths (
 chr TEXT PRIMARY KEY,
 length INTEGER
);
CREATE TABLE chromosome_features (
 id INTEGER REFERENCES sgd(id),
 chromosome TEXT,
 start INTEGER,
 feature_description TEXT
);
CREATE TABLE ec (
 id INTEGER REFERENCES sgd(id),
 ec_number TEXT
);
CREATE TABLE gene2alias (
 id INTEGER REFERENCES sgd(id),
 alias TEXT
);
CREATE TABLE gene2systematic (
 gene_name TEXT,
 systematic_name TEXT
);
CREATE TABLE go_bp (
 id INTEGER REFERENCES sgd(id),
 go_id TEXT,
 evidence TEXT
);
CREATE TABLE go_cc (
 id INTEGER REFERENCES sgd(id),
 go_id TEXT,
 evidence TEXT
);
CREATE TABLE go_mf (
 id INTEGER REFERENCES sgd(id),
 go_id TEXT,
 evidence TEXT
);
CREATE TABLE interpro (
 id INTEGER REFERENCES sgd(id),
 interpro_id TEXT
);
CREATE TABLE kegg (
 id INTEGER REFERENCES sgd(id),
 kegg_id TEXT
);
CREATE TABLE map_metadata (
 map_name TEXT,
 source_name TEXT,
 source_url TEXT,
 source_date TEXT
);
CREATE TABLE metadata (
        name TEXT,
        value TEXT
);
CREATE TABLE pfam (
 id INTEGER REFERENCES sgd(id),
 pfam_id TEXT
);
CREATE TABLE pubmed (
 id INTEGER REFERENCES sgd(id),
 pubmed_id TEXT
);
CREATE TABLE qcdata (
 map_name TEXT,
 count INTEGER,
 UNIQUE(map_name)
);
CREATE TABLE reject_orf (
 systematic_name TEXT
);
CREATE TABLE smart (
 id INTEGER REFERENCES sgd(id),
 smart_id TEXT
);
CREATE INDEX cf1 on chromosome_features(id);
CREATE INDEX e1 ON ec(id);
CREATE INDEX ga1 on gene2alias(id);
CREATE INDEX go1 on go_bp(id);
CREATE INDEX go2 on go_mf(id);
CREATE INDEX go3 on go_cc(id);
CREATE INDEX go4 on go_bp(go_id);
CREATE INDEX go5 on go_mf(go_id);
CREATE INDEX go6 on go_cc(go_id);
CREATE INDEX in1 ON interpro(id);
CREATE INDEX k1 ON kegg(id);
CREATE INDEX pf1 ON pfam(id);
CREATE INDEX pm1 ON pubmed(id);
CREATE INDEX s1 on sgd(sgd_id);
CREATE INDEX s2 on sgd(systematic_name);
CREATE INDEX sm1 ON smart(id);

