
process FASTQC {
    container "biocontainers/fastqc:v0.11.9_cv8"
    publishDir "output/raw_reads",  mode: 'symlink', overwrite: true

    input:
    tuple val(meta),path(reads)

    output:
    path "*.html"
    path "*.zip"
    path:

    script:
    """

    fastqc ${meta.id}_1.fastq.gz ${meta.id}_2.fastq.gz
    """
}

workflow{
    ch_input = channel.fromList(params.raw_reads).map(it->[[id:it.sample_name],[it.fastq1,it.fastq2]])
    ch_input.view()
    FASTQC(ch_input)
}