process kraken2 {
    publishDir "output/kraken2/${meta.id}", mode: 'symlink', overwrite: true

    input:
    tuple val(meta),path(reads)
    path  db

    output:
    tuple val(meta), path('*.classified{.,_}*')     , optional:true, emit: classified_reads_fastq
    tuple val(meta), path('*.unclassified#{.,_}*')   , optional:true, emit: unclassified_reads_fastq
    tuple val(meta), path('*classifiedreads.txt')   , optional:true, emit: classified_reads_assignment
    tuple val(meta), path('*report.txt')                           , emit: report

    script:
    """
    PATH=/home/jovyan/.conda/envs/kraken2/bin:\$PATH
    kraken2 \
        --db $db \
        --threads $task.cpus \
        --report ${meta.id}.kraken2.report.txt \
        --gzip-compressed \
        --unclassified-out ${meta.id}.unclassified#.fastq \
        --classified-out ${meta.id}.classified#.fastq \
        --output ${meta.id}.kraken2.classifiedreads.txt \
        ${reads}
    """

    stub:
    """
    
    """
}

process bracken {
    publishDir "output/bracken/${meta.id}", mode: 'copy', overwrite: true

    input:
    tuple val(meta),path(report)
    path(db)

    output:
    tuple val(meta), path("${meta.id}.*.tsv")        , emit: reports

    script:
    """
    PATH=/home/jovyan/.conda/envs/kraken2/bin:\$PATH
    bracken \\
        -l D \\
        -d '${db}' \\
        -i '${report}' \\
        -o '${meta.id}.D.kraken.tsv'
    bracken \\
        -l K \\
        -d '${db}' \\
        -i '${report}' \\
        -o '${meta.id}.K.kraken.tsv'
    bracken \\
        -l P \\
        -d '${db}' \\
        -i '${report}' \\
        -o '${meta.id}.P.kraken.tsv'

    bracken \\
        -l C \\
        -d '${db}' \\
        -i '${report}' \\
        -o '${meta.id}.C.kraken.tsv'

    bracken \\
        -l O \\
        -d '${db}' \\
        -i '${report}' \\
        -o '${meta.id}.O.kraken.tsv'

    bracken \\
        -l F \\
        -d '${db}' \\
        -i '${report}' \\
        -o '${meta.id}.F.kraken.tsv'


    bracken \\
         -l G  \\
        -d '${db}' \\
        -i '${report}' \\
        -o '${meta.id}.G.kraken.tsv'
    bracken \\
         -l S  \\
        -d '${db}' \\
        -i '${report}' \\
        -o '${meta.id}.S.kraken.tsv'

    bracken \\
        -d '${db}' \\
        -l S1 \\
        -i '${report}' \\
        -o '${meta.id}.S1.kraken.tsv'

    
    """

    stub:
    """
    
    """
}
workflow{
    ch_input = channel.fromList(params.remove_host_reads).map(it->[[id:it.sample_name],[it.fastq1,it.fastq2]])
    ch_kraken2_res = kraken2(ch_input, params.kraken_database.path)
    ch_bracken_res = bracken(ch_kraken2_res.report,params.kraken_database.path)


}