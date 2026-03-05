process bowtie2_index {
    container "registry.cn-hangzhou.aliyuncs.com/wybioinfo/bowtie2:latest"
    publishDir "output/bowtie2_index", mode: 'symlink', overwrite: true

    input:
    tuple val(meta),path(fasta)

    output:
    tuple val(meta),path("*") ,emit:index

    script:
    """

    bowtie2-build --threads ${task.cpus} ${fasta} ${meta.id}

    """

    stub:
    """
    
    """
}
workflow{
    // ch_genome_assembly_input =  channel.of([params.fasta]).map(it->[[id:it.sample_name],it.fasta])
    ch_bowtie2_index_out =  bowtie2_index([[id:params.fasta.file_name],params.fasta.fa])
    ch_bowtie2_index_out.index.view()
}