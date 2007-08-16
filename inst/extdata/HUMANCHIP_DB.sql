--
-- HUMAN_DB schema
-- ===============
--
-- Differences with the schema currently in use:
--
--   o renamed "qcdata" table -> "map_counts"
--   o renamed internal id "id" -> "_id"
--   o renamed chrlengths.chr col -> "chromosome"
--   o changed order of table definitions so referenced tables are created
--     before referecing tables
--   o added a PRIMARY KEY constraint on probes.probe_id and metadata.name
--   o added UNIQUE constraint on genes.gene_id and gene_info._id
--   o explicit use of the NULL or NOT NULL constraint on every column (except
--     on primary key cols that are always NOT NULL)
--   o put "_id" col in last position in "probes" table
--   o replaced TEXT type by standard SQL character types (see "Table of
--     character types" below)
--
-- Table of character types:
--   Note: MySQL supports VARCHAR up to 255 chars only
--
--     Description                Type           Columns
--     ------------------------   ------------   ---------
--     Entrez Gene ID             VARCHAR(10)    genes.gene_id
--     manufacturer ID            VARCHAR(80)    probes.probe_id
--     GenBank accession number   VARCHAR(20)    probes.accession, accessions.accession
--     gene symbol or alias       VARCHAR(80)    alias.alias_symbol, gene_info.symbol
--     chromosome name            VARCHAR(2)     chromosomes.chromosome
--                                               chromosome_locations.chromosome
--                                               chrlengths.chromosome
--     gene name                  VARCHAR(255)   gene_info.gene_name
--     GO ID                      CHAR(10)       go_[bp|cc|mf].go_id
--     GO evidence code           CHAR(3)        go_[bp|cc|mf].evidence
--     KEGG ID                    CHAR(5)        kegg.kegg_id
--     OMIM ID                    CHAR(6)        omim.omim_id
--     IPI accession number       CHAR(11)       pfam.ipi_id, prosite.ipi_id
--     Pfam ID                    CHAR(7)        pfam.pfam_id
--     PROSITE ID                 CHAR(7)        prosite.prosite_id
--     PubMed ID                  VARCHAR(10)    pubmed.pubmed_id
--     RefSeq accession number    VARCHAR(20)    refseq.accession
--     UniGene ID                 VARCHAR(10)    unigene.unigene_id
--

--
-- The "genes" table is the central table.
-- TODO: merge the "gene_info" table to it.
--
CREATE TABLE genes (
  _id INTEGER PRIMARY KEY,
  gene_id VARCHAR(10) UNIQUE NOT NULL
);

--
-- The "probes" table
--
CREATE TABLE probes (
  probe_id VARCHAR(80) PRIMARY KEY,
  accession VARCHAR(20) NOT NULL,
  _id INTEGER NULL REFERENCES genes (_id)
);

CREATE TABLE accessions (
  _id INTEGER NOT NULL REFERENCES genes (_id),
  accession VARCHAR(20) NOT NULL
);
CREATE TABLE alias (
  _id INTEGER NOT NULL REFERENCES genes (_id),
  alias_symbol VARCHAR(80) NOT NULL
);
CREATE TABLE chromosomes (
  _id INTEGER NOT NULL REFERENCES genes (_id),
  chromosome VARCHAR(2) NOT NULL
);
CREATE TABLE chromosome_locations (
  _id INTEGER NOT NULL REFERENCES genes (_id),
  chromosome VARCHAR(2) NOT NULL,
  start_location INTEGER NOT NULL
);
CREATE TABLE cytogenetic_locations (
  _id INTEGER NOT NULL REFERENCES genes (_id),
  cytogenetic_location VARCHAR(20) NOT NULL
);
CREATE TABLE ec (
  _id INTEGER NOT NULL REFERENCES genes (_id),
  ec_number VARCHAR(20) NOT NULL
);

--
-- TODO: merge the "gene_info" table with the "gene" table.
-- We currently have a few symbols that are mapped to 2 or 3 genes.
-- If this can't be avoided then we'll have to live with it and we
-- can't put a UNIQUE constraint on the "symbol" column.
-- But if this is an error in our current sqlite files and we can
-- get rid of it then we can put a UNIQUE constraint on the "symbol"
-- so this will never happen again.
--
CREATE TABLE gene_info (
  _id INTEGER UNIQUE NOT NULL REFERENCES genes (_id),
  symbol VARCHAR(80) NOT NULL,
  gene_name VARCHAR(255) NOT NULL
);
CREATE TABLE go_bp (
  _id INTEGER NOT NULL REFERENCES genes (_id),
  go_id CHAR(10) NOT NULL,
  evidence CHAR(3) NOT NULL
);
CREATE TABLE go_bp_all ( 
  _id INTEGER NOT NULL REFERENCES genes (_id),
  go_id CHAR(10) NOT NULL,
  evidence CHAR(3) NOT NULL
);
CREATE TABLE go_cc (
  _id INTEGER NOT NULL REFERENCES genes (_id),
  go_id CHAR(10) NOT NULL,
  evidence CHAR(3) NOT NULL
);
CREATE TABLE go_cc_all (
  _id INTEGER NOT NULL REFERENCES genes (_id),
  go_id CHAR(10) NOT NULL,
  evidence CHAR(3) NOT NULL
);
CREATE TABLE go_mf (
  _id INTEGER NOT NULL REFERENCES genes (_id),
  go_id CHAR(10) NOT NULL,
  evidence CHAR(3) NOT NULL
);
CREATE TABLE go_mf_all (
  _id INTEGER NOT NULL REFERENCES genes (_id),
  go_id CHAR(10) NOT NULL,
  evidence CHAR(3) NOT NULL
);
CREATE TABLE kegg (
  _id INTEGER NOT NULL REFERENCES genes (_id),
  kegg_id CHAR(5) NOT NULL
);
CREATE TABLE omim (
  _id INTEGER NOT NULL REFERENCES genes (_id),
  omim_id CHAR(6) NOT NULL
);
CREATE TABLE pfam (
  _id INTEGER NOT NULL REFERENCES genes (_id),
  ipi_id CHAR(11) NOT NULL,
  pfam_id CHAR(7) NULL
);
CREATE TABLE prosite (
  _id INTEGER NOT NULL REFERENCES genes (_id),
  ipi_id CHAR(11) NOT NULL,
  prosite_id CHAR(7) NULL
);
CREATE TABLE pubmed (
  _id INTEGER NOT NULL REFERENCES genes (_id),
  pubmed_id VARCHAR(10) NOT NULL
);
CREATE TABLE refseq (
  _id INTEGER NOT NULL REFERENCES genes (_id),
  accession VARCHAR(20) NOT NULL
);
CREATE TABLE unigene (
  _id INTEGER NOT NULL REFERENCES genes (_id),
  unigene_id VARCHAR(10) NOT NULL
);

CREATE TABLE chrlengths (
  chromosome VARCHAR(2) PRIMARY KEY,
  length INTEGER NOT NULL
);


--
-- Metadata tables
--

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

