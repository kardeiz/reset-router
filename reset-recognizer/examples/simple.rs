fn main() -> Result<(), Box<dyn std::error::Error>> {
    let router = reset_recognizer::Router::build()
        .add(r"^/posts/(\d+)/comments/(\d+)$", "comment".to_string())
        .add(r"^/posts/(\d+)/comments$", "comments".to_string())
        .add(r"^/posts/(\d+)$", "post".to_string())
        .add(r"^/posts$", "posts".to_string())
        .add(r"^/comments$", "comments2".to_string())
        .add(r"^/comments/(\d+)$", "comment2".to_string())
        .add_with_priority(r"^/(.+)$", -1, "not_found".to_string())
        .finish()?;

    let matched = router.recognize("/posts/100/comments/200")?;

    let (post_id, comment_id) = matched.captures.parsed::<(i32, i32)>()?;

    std::thread::spawn(move || {
        println!("{:?}", (&post_id, &comment_id));
    }).join().unwrap();

    

    Ok(())
}