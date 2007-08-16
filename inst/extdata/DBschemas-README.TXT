
The *_DB.sql files in this directory contain the new DB schemas.

The new schemas are portable (tested on SQLite and PostgreSQL only so far,
still need to test them on MySQL and Oracle), self-describing, easier to
read and easier to understand than the current ones.


Summary of modifications from the current schemas
=================================================

  o renamed "qcdata" table -> "map_counts"
  o renamed internal id "id" -> "_id"
  o renamed chrlengths.chr col -> "chromosome"
  o made sure that referenced tables are created before referencing tables
  o added a PRIMARY KEY constraint on probes.probe_id and metadata.name
  o added UNIQUE constraint on genes.gene_id and gene_info._id
  o explicit use of the NULL or NOT NULL constraint on every column (except
    on primary key cols that are implicitly NOT NULL)
  o put "_id" col in last position in "probes" table
  o replaced the non-standard TEXT type by one of the standard SQL
    character types (see "Table of character types" below)
  o replaced foreign key def
      REFERENCES genes(_id)
    with
      REFERENCES genes


Table of character types
========================

  Description                Type           Columns
  ------------------------   ------------   ---------
  Entrez Gene ID             VARCHAR(10)    genes.gene_id
  manufacturer ID            VARCHAR(80)    probes.probe_id
  GenBank accession number   VARCHAR(20)    probes.accession, accessions.accession
  gene symbol or alias       VARCHAR(80)    alias.alias_symbol, gene_info.symbol
  chromosome name            VARCHAR(2)     chromosomes.chromosome
                                            chromosome_locations.chromosome
                                            chrlengths.chromosome
  gene name                  VARCHAR(255)   gene_info.gene_name
  GO ID                      CHAR(10)       go_[bp|cc|mf].go_id
  GO evidence code           CHAR(3)        go_[bp|cc|mf].evidence
  KEGG ID                    CHAR(5)        kegg.kegg_id
  OMIM ID                    CHAR(6)        omim.omim_id
  IPI accession number       CHAR(11)       pfam.ipi_id, prosite.ipi_id
  Pfam ID                    CHAR(7)        pfam.pfam_id
  PROSITE ID                 CHAR(7)        prosite.prosite_id
  PubMed ID                  VARCHAR(10)    pubmed.pubmed_id
  RefSeq accession number    VARCHAR(20)    refseq.accession
  UniGene ID                 VARCHAR(10)    unigene.unigene_id

Notes: - MySQL supports VARCHAR up to 255 chars only.
       - These types should be used consistently across all schemas.

