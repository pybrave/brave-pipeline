
process vsearch_merge_pairs {
    tag "$meta.id"
    publishDir "output/vsearch_merge_pairs", mode: 'symlink', overwrite: true
    container "registry.cn-hangzhou.aliyuncs.com/wybioinfo/vsearch:v1.28"

    input:
    tuple val(meta), path(reads)

    output:
    path "${meta.id}.merged.fa"

    script:
    """
    gunzip -c ${reads[0]} > r1.fastq
    gunzip -c ${reads[1]} > r2.fastq
    vsearch --fastq_mergepairs r1.fastq \
            --reverse r2.fastq  \
            --fastaout  ${meta.id}.merged.fa \
            --relabel ${meta.id}.
    """
}

workflow{
    ch_input = channel.fromList(params.clean_reads).map(it->[[id:it.sample_name],[it.fastq1,it.fastq2]])
    vsearch_merge_pairs(ch_input )
}