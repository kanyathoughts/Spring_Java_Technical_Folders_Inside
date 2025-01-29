const getVersion = async function () {
	const response = await fetch(getBaseUrl() + 'api/v1/version');
	if (response.ok) {
		return (await response.json())['version']
	}
	return 'could not be retrieved. Ensure that mining-server is running and "HTML Display name" on the realm is set to the correct URL.';
}

const start = async function () {
	var version = await getVersion();
	document.getElementById("copyright-note-holder").innerHTML = 'Copyright &copy; ' + new Date().getFullYear() + ' by Deloitte Consulting LLP, all rights reserved.';
	document.getElementById("version-holder").innerHTML = version;
}

function getBaseUrl() {
	const meta = document.getElementsByTagName('meta');
	for (const item of meta) {
		const metaName = item.getAttribute('name');
		if (metaName === 'mining-base-url') {
			return item.getAttribute('content')
		}
	}
	return undefined;
}
start();
