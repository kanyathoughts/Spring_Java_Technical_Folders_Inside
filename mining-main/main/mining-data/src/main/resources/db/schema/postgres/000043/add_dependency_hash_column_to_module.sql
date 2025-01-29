ALTER TABLE "module" ADD COLUMN "dependency_hash" varchar(32);
UPDATE module m set dependency_hash = (select md5(format('%s,%s,%s,%s,%s,%s,%s',src_linkhash, src_offset, src_length, dst_linkhash, dst_offset, dst_length, relationship_type)) from  
    (
    SELECT 
         array_agg(m_src.link_hash ORDER BY m_src.link_hash) as src_linkhash,
         array_agg((mr.src_location).offset ORDER BY (mr.src_location).offset) as src_offset,
         array_agg((mr.src_location).length ORDER BY (mr.src_location).length) as src_length, 
         array_agg(m_dst.link_hash ORDER BY m_dst.link_hash) as dst_linkhash,
         array_agg((mr.dst_location).offset ORDER BY (mr.dst_location).offset) as dst_offset,
         array_agg((mr.dst_location).length ORDER BY (mr.dst_location).length) as dst_length,
         array_agg(mr."type" ORDER BY mr."type") as relationship_type
    FROM
        module_relationship mr 
    JOIN 
        module m_src ON m_src.uid = mr.src
    JOIN 
        module m_dst ON m_dst.uid = mr.dst
        where m.uid = mr.src or m.uid = mr.dst
    ) as agg_values)
where m.project IN (SELECT uid FROM project);
