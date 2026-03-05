// include {bowtie2} from './modules/bowtie2.nf'
process samtools_remove_hosts {
    publishDir "output/remove_hosts/${meta.id}", mode: 'symlink', overwrite: true

    input:
    tuple val(meta),path(bam)

    output:
    tuple val(meta),path("*.fastq.gz") ,emit:fastq

    script:
    """
    export PATH=/home/jovyan/.conda/envs/bowtie2/bin:\$PATH

    samtools view  \
        -@ task.cpus \
        -b -f 12 -F 256 \
        ${bam} > ${meta.id}_bothReadsUnmapped.bam 
    
    samtools sort -n  -@ ${task.cpus} \
        ${meta.id}_bothReadsUnmapped.bam \
         -o ${meta.id}_bothReadsUnmapped_sorted.bam

    samtools fastq -@ ${task.cpus} ${meta.id}_bothReadsUnmapped_sorted.bam \
        -1 ${meta.id}_host_removed_R1.fastq.gz \
        -2 ${meta.id}_host_removed_R2.fastq.gz \
        -0 /dev/null -s /dev/null -n

    """

    stub:
    """
    
    """
}


process bowtie2 {
    tag "$meta.id"
    publishDir "output/remove_hosts/${meta.id}", mode: 'symlink', overwrite: true
    // publishDir "/data2/wangyang/storeDir/test/${meta.id}", mode: 'copy', overwrite: true

    // storeDir '/data2/wangyang/storeDir/test2'

    input:
    tuple val(meta),path(reads)
    val(reference)

    output:
    tuple val(meta), path("*.{bam,sam}"), emit: aligned, optional:true
    tuple val(meta), path("*.log")      , emit: log
    tuple val(meta), path("*fastq.*.gz")  , emit: fastq, optional:true


    script:
    """
    export PATH=/home/jovyan/.conda/envs/bowtie2/bin:\$PATH
    bowtie2 \
        -x ${reference} \
        -1 ${reads[0]} -2 ${reads[1]} \
        --threads ${task.cpus} \
        --un-conc  ${meta.id}.unmapped.fastq.gz \
        2> >(tee ${meta.id}.bowtie2.log >&2) \
        | samtools view --threads $task.cpus -bS -o ${meta.id}.bam
    """

    stub:
    """
    
    """
}

workflow remove_hosts {
    take:
    ch_input

    main:
    ch_bowtie2_host_index = Channel.value(params.genome_index.path)
    ch_bowtie2_res = bowtie2(ch_input, ch_bowtie2_host_index)    
    ch_samtools_remove_hosts_res = samtools_remove_hosts(ch_bowtie2_res.aligned)

    emit:
    ch_samtools_remove_hosts_res
}


workflow  {
    ch_input = channel.fromList(params.clean_reads).map(it->[[id:it.sample_name],[it.fastq1,it.fastq2]])
    ch_remove_hosts_reads = remove_hosts(ch_input)
}