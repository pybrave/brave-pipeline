process seqkit {
    publishDir "output", mode: 'symlink', overwrite: true

    input:
    path(fastq_files)
    val(name)
    val(suffix)

    output:
    path("*.tsv"),emit:tsv

    script:
    """
    export PATH=/home/jovyan/.conda/envs/seqkit/bin:\$PATH
    seqkit -j ${task.cpus} stats ${suffix} -T  -a > ${name}.tsv

    """

    stub:
    """
    """
}
workflow{
    ch_input = channel.fromList(params.raw_reads).map(it->[[id:it.sample_name],[it.fastq1,it.fastq2]])

    seqkit(ch_input.collect{it[1]},"seqkit","*.fastp.fastq.gz")

}