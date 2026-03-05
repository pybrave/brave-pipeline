process bowtie2 {
    tag "$meta.id"
    publishDir "output/bowtie2/${meta.id}", mode: 'symlink', overwrite: true
    container "registry.cn-hangzhou.aliyuncs.com/wybioinfo/bowtie2:latest"

    input:
    tuple val(meta),path(reads)
    val(reference)

    output:
    tuple val(meta), path("*.{bam,sam}"), emit: aligned, optional:true
    tuple val(meta), path("*.log")      , emit: log
    tuple val(meta), path("*fastq.*.gz")  , emit: fastq, optional:true


    script:
    """
    bowtie2 \
        -x ${reference} \
        -1 ${reads[0]} -2 ${reads[1]} \
        --threads ${task.cpus} \
        2> >(tee ${meta.id}.bowtie2.log >&2) \
        | samtools view --threads $task.cpus -bS \
        | samtools sort -@ ${task.cpus} -o ${meta.id}.sorted.bam
    samtools index ${meta.id}.sorted.bam
    """

    stub:
    """
    
    """
}

workflow{
    ch_input = channel.fromList(params.clean_reads).map(it->[[id:it.sample_name],[it.fastq1,it.fastq2]])
    bowtie2_index = channel.value(params.bowtie2_index.index.replace(".rev.2.bt2","") )
    // bowtie2_index.view()
    bowtie2(ch_input, bowtie2_index)
}